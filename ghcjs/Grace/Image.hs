-- |

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Grace.Image where

import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Control.Concurrent as Concurrent
import qualified JavaScript.Web.Canvas as Canvas
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import GHCJS.Marshal (fromJSVal, toJSVal)
import GHCJS.Foreign.Callback (Callback, asyncCallback)
import qualified JavaScript.TypedArray as TypedArray
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import GHCJS.Foreign.Callback (Callback)
import GHCJS.Types (JSVal)

data Img = Img { base64Image :: Text }
  deriving (Eq, Show)

-- TODO: Deduplicate this from try-grace/Main.hs.
foreign import javascript unsafe "console.log($1)"
    consoleLog_ :: JSString -> IO ()

consoleLog :: MonadIO io => Text -> io ()
consoleLog t = liftIO . consoleLog_ . JSString.pack $ Text.unpack t

consoleShow :: Show a => Text -> a -> IO ()
consoleShow prefix x = consoleLog $ prefix <> " " <> Text.pack (show x)

-- foreign import javascript unsafe "$r = new Array(); for (chan of [1,2,3]) { for (r of Array($1).keys()) { for (c of Array(2).keys()) { i = c*4 + r * $1 * 4 + chan; $r.push($3[i/255.0]); }  } }"
--     imageDataToChannelMajorTensor_ :: Int -> Int -> JSVal -> IO JSVal


foreign import javascript unsafe "$r = new Array(); for (r = 0; r < $1; r++) { for (c = 0; c < $2; c++) { i = c*4 + r * $1 * 4; red = ($3.data)[i]; green = ($3.data)[i+1]; blue = ($3.data)[i+2]; color_sum = red + green + blue; avg = color_sum * 0.001307; ($r).push(avg)};  }"
    imageDataToMonochromeTensor_ :: Int -> Int -> JSVal -> IO JSVal

foreign import javascript unsafe "$r = new Array(); w = $1.width; for (r = 0; r < $1.height; r++) { for (c = 0; c < w; c++) { i = c*4 + r * w * 4; d = $1.data; ($r).push(d[i] * 0.001307); ($r).push(d[i+1] * 0.001307); ($r).push(d[i+2] * 0.001307) }  };"
    imageDataToPixelMajorTensor_ :: JSVal -> IO JSVal

foreign import javascript unsafe "ctx = $1.getContext('2d'); iData = ctx.getImageData(0,0,$3,$4); d = iData.data; for (r = 0; r < $4; r++) { for (c = 0; c < $3; c++) { i = c * 3 + r * $3 * 3; j = c * 4 + r * $3 * 4; d[j] = ($2)[i] * 255; d[j+1] = ($2)[i+1] * 255; d[j+2] = ($2)[i+2] * 255; d[j+3] = 255; }}; iData.data = d; ctx.putImageData(iData,0,0);"
  drawTensorToCanvas :: Canvas.Canvas -> JSVal -> Int -> Int -> IO ()

foreign import javascript unsafe "ctx = $1.getContext('2d'); iData = ctx.getImageData(0,0,$3,$4); d = iData.data; for (r = 0; r < $4; r++) { for (c = 0; c < $3; c++) { i = c  + r * $3; j = c * 4 + r * $3 * 4; d[j] = ($2)[i] * 255; d[j+1] = ($2)[i] * 255; d[j+2] = ($2)[i] * 255; d[j+3] = 255; }}; iData.data = d; ctx.putImageData(iData,0,0);"
  drawMonochromeTensorToCanvas :: Canvas.Canvas -> JSVal -> Int -> Int -> IO ()

-- imageDataToChannelMajorTensor :: TypedArray.Uint8ClampedArray -> Int -> Int -> IO [Float]
-- imageDataToChannelMajorTensor arr width height = do
--   let
--     channels = [1,2,3] -- Position 0 is S. 1 is R, 2 is G, 3 is B.
--     rows = [0..width]
--     cols = [0..height]
--     indices = (\chan row col -> col * 4 + (row * width * 4) + chan) <$> channels <*> rows <*> cols
--   traverse (\ind -> (/255.0) . realToFrac <$> TypedArray.index ind arr) indices
  

-- General idea:
--  Receive a base64 encoded image (assume jpeg)
--  Construct an Image html element
--  Draw that image onto a (hidden) canvas
--  Get the imagedata from the canvas as a UInt8ClampedArray
--  Rearrange the bytes of the UInt8ClampedArray from pixel-major to channel-major
--
-- TODO: hack: this function assumes the output tensor is channel-major,
-- and we cheat by putting it into a batch of 1. We also assume that
-- the second dimension is the channel count, and only handle 1 channel
-- (monochrome) or 3 channels (rgb). We assume the image is a jpeg.
imageToTensor :: Img -> [Int] -> Either String ((Int,Int), [Float])

-- No image.
imageToTensor (Img {base64Image = ""}) [nBatch,nChans,nRows,nCols] = Left "no image" -- Right ((nCols, nRows), replicate (nBatch * nChans * nRows * nCols) 0.0)

-- Monochrome.
imageToTensor img@(Img {base64Image}) [1,1,nRows,nCols] =
  unsafePerformIO $  do
    canvas <- canvasWithImage nCols nRows img
    imageData <- canvasImageData canvas nCols nRows
    tensorVals <- imageDataToMonochromeTensor_ nCols nRows imageData
    Just v <- fromJSVal tensorVals
    return $ Right ((nCols,nRows), v)

-- Pixel-major RGB
imageToTensor img@(Img {base64Image}) [-1,-1,-1,3] =
  unsafePerformIO $ do
    t0 <- getCurrentTime
    consoleLog "about to canvasWithImageNativeSize"
    (canvas, width, height) <- canvasWithImageNativeSize img
    consoleShow "width:" width
    consoleShow "height:" height
    imageData <- canvasImageDataNativeSize canvas
    tensorVals <- imageDataToPixelMajorTensor_ imageData
    Just v <- fromJSVal tensorVals
    t1 <- getCurrentTime
    consoleLog (Text.pack $ "imageToTensor took: " <> show (diffUTCTime t1 t0))
    return $ Right ((width, height), v)

imageToTensor _ _ = Left "TODO: Hack: Only batch-size-one is supported"

imageFromTensor :: [Int] ->[Float] ->  Img

-- Pixel-major rgb. 
imageFromTensor [_,nRows,nCols,3] rgbValues =
  unsafePerformIO $ do
    consoleLog "imageFromTensor"
    jsValues <- toJSVal rgbValues
    canvas <- Canvas.create nCols nRows
    -- appendChild_ canvas -- TODO: temporary, debugging
    drawTensorToCanvas canvas jsValues nCols nRows
    Img <$> toDataUrl canvas
imageFromTensor [1,1,nRows,nCols] monochromeValues =
  unsafePerformIO $ do
    jsValues <- toJSVal monochromeValues
    canvas <- Canvas.create nCols nRows
    drawMonochromeTensorToCanvas canvas jsValues nCols nRows
    Img <$> toDataUrl canvas
imageFromTensor shape monochromeValues =
  unsafePerformIO $ do
    consoleLog (Text.pack $ "Don't know how to get image from tensor shape " <> show shape)
    undefined

foreign import javascript unsafe "$1.toDataURL('image/jpeg', 0.5)"
    toDataUrl_ :: Canvas.Canvas -> IO JSString

foreign import javascript unsafe "c = document.getElementsByClassName('container')[0]; c.appendChild($1);"
    appendChild_ :: Canvas.Canvas -> IO ()

toDataUrl :: Canvas.Canvas -> IO Text
toDataUrl canvas = Text.pack . JSString.unpack <$> toDataUrl_ canvas
    
  

resizeImage :: Img -> IO Img
resizeImage (Img base64Bytes) = do
  undefined

foreign import javascript unsafe "$1.addEventListener('load', $2, false)"
  onLoad_ :: Canvas.Image -> Callback (IO ()) -> IO ()

canvasWithImage :: Int -> Int -> Img -> IO Canvas.Canvas
canvasWithImage width height img = liftIO $ do
  imgReady <- MVar.newEmptyMVar
  canvas <- Canvas.create width height
  imgJs <- createImage img
  callback <- asyncCallback $ do
    consoleLog "fixedsize: callback about to put mvar ()"
    MVar.putMVar imgReady ()
  consoleLog "waiting for mvar"
  onLoad_ imgJs callback
  () <- MVar.takeMVar imgReady
  ctx <- Canvas.getContext canvas
  Canvas.drawImage imgJs 0 0 width height ctx
  return canvas

canvasWithImageNativeSize :: Img -> IO (Canvas.Canvas, Int, Int)
canvasWithImageNativeSize img = liftIO $ do
  imgReady <- MVar.newEmptyMVar
  callback <- asyncCallback $ do
    consoleLog "nativesize: callback about to put mvar ()"
    MVar.putMVar imgReady ()
  imgJs <- createImage img
  consoleLog "waiting for mvar"
  onLoad_ imgJs callback
  () <- MVar.takeMVar imgReady
  width <- imgWidth_ imgJs
  height <- imgHeight_ imgJs
  canvas <- Canvas.create width height
  ctx <- Canvas.getContext canvas
  Canvas.drawImage imgJs 0 0 width height ctx
  return (canvas, width, height)


foreign import javascript unsafe "ctx = $1.getContext('2d', {alpha: false}); $r = ctx.getImageData(0,0,$2,$3);"
  canvasImageData :: Canvas.Canvas -> Int -> Int -> IO JSVal

foreign import javascript unsafe "w = $1.width; h = $1.height; ctx = $1.getContext('2d', {alpha: false}); $r = ctx.getImageData(0,0,w,h);"
  canvasImageDataNativeSize :: Canvas.Canvas -> IO JSVal

foreign import javascript unsafe "$r = document.createElement(\"img\"); ($r).src=($1);"
  createImageWithSrc_ :: JSString -> IO JSVal

foreign import javascript unsafe "$1.width"
  imgWidth_ :: Canvas.Image -> IO Int

foreign import javascript unsafe "$1.height"
  imgHeight_ :: Canvas.Image -> IO Int

createImage :: MonadIO m => Img -> m Canvas.Image
createImage (Img base64Bytes) = do
  jsVal <- liftIO $ createImageWithSrc_ (fromText base64Bytes)
  return $ unsafeCoerce jsVal
  

fromText :: Text -> JSString
fromText = JSString.pack . Text.unpack


foreign import javascript unsafe "document.createElement($1)"
    createElement_ :: JSString -> IO JSVal

createElement :: MonadIO io => Text -> io JSVal
createElement a = liftIO (createElement_ (fromText a))

foreign import javascript unsafe "document.getElementById($1)"
    getElementById_ :: JSString -> IO JSVal


getElementById :: MonadIO io => Text -> io JSVal
getElementById a = liftIO (getElementById_ (fromText a))

  
