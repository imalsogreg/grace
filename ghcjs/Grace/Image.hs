-- |

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Grace.Image where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Control.Concurrent as Concurrent
import qualified JavaScript.Web.Canvas as Canvas
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import GHCJS.Marshal (fromJSVal)
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

-- foreign import javascript unsafe "$r = new Array(); for (chan of [1,2,3]) { for (r of Array($1).keys()) { for (c of Array(2).keys()) { i = c*4 + r * $1 * 4 + chan; $r.push($3[i/255.0]); }  } }"
--     imageDataToChannelMajorTensor_ :: Int -> Int -> JSVal -> IO JSVal


foreign import javascript unsafe "$r = new Array(); for (r = 0; r < $1; r++) { for (c = 0; c < $2; c++) { i = c*4 + r * $1 * 4; red = ($3.data)[i]; green = ($3.data)[i+1]; blue = ($3.data)[i+2]; color_sum = red + green + blue; avg = color_sum * 0.001307; ($r).push(avg)};  }"
    imageDataToMonochromeTensor_ :: Int -> Int -> JSVal -> IO JSVal

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
imageToTensor :: Img -> [Int] -> Either String [Float]

-- No image.
imageToTensor (Img {base64Image = ""}) [nBatch,nChans,nRows,nCols] = Right (replicate (nBatch * nChans * nRows * nCols) 0.0)

-- Monochrome.
imageToTensor img@(Img {base64Image}) [1,1,nRows,nCols] =
  unsafePerformIO $  do
    canvas <- canvasWithImage nCols nRows img
    imageData <- canvasImageData canvas nCols nRows
    tensorVals <- imageDataToMonochromeTensor_ nCols nRows imageData
    Just v <- fromJSVal tensorVals
    consoleLog (Text.pack $ show v)
    return $ Right v

-- Pixel-major RGB
imageToTensor (Img {base64Image}) [-1,-1,-1,3] =
  unsafePerformIO $ do
    (canvas, width, height) <- canvasWithImageNativeSize img
    imageData <- canvasImageDataNativeSize canvas
    tensorVals <- imageDataToPixelMajorTensor_ 
imageToTensor _ _ = Left "TODO: Hack: Only batch-size-one is supported"

resizeImage :: Img -> IO Img
resizeImage (Img base64Bytes) = do
  undefined

canvasWithImage :: Int -> Int -> Img -> IO Canvas.Canvas
canvasWithImage width height img = liftIO $ do
  canvas <- Canvas.create width height
  imgJs <- createImage img
  Concurrent.threadDelay 500000
  ctx <- Canvas.getContext canvas
  Canvas.drawImage imgJs 0 0 width height ctx
  return canvas

canvasWithImageNativeSize :: Img -> IO (Canvas.Canvas, Int, Int)
canvasWithImageNativeSize img = liftIO $ do
  imgJs <- createImage img
  Concurrent.threadDelay 500000
  width <- imgWidth_ imgJs
  height <- imgHeight_ imgJs
  canvas <- Canvas.create width height
  Canvas.drawImage imgJs 0 0 width height ctx
  return (canvas, width, height)


foreign import javascript unsafe "ctx = $1.getContext('2d', {alpha: false}); $r = ctx.getImageData(0,0,$2,$3);"
  canvasImageData :: Canvas.Canvas -> Int -> Int -> IO JSVal

foreign import javascript unsafe "$r = document.createElement(\"img\"); ($r).src=($1);"
  createImageWithSrc_ :: JSString -> IO JSVal

foreign import javascript unsafe "$1.width"
  imgWidth_ :: JSVal -> IO Int

foreign import javascript unsafe "$1.height"
  imgHeight_ :: JSVal -> IO Int

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

  
