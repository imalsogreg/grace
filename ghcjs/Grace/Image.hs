-- |

{-# LANGUAGE NamedFieldPuns #-}

module Grace.Image where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified JavaScript.Web.Canvas as Canvas
import qualified Data.Text as Text
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import GHCJS.Foreign.Callback (Callback)
import GHCJS.Types (JSVal)

data Img = Img { base64Image :: Text }
  deriving (Eq, Show)

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
imageToTensor img@(Img {base64Image}) [1,nChans,nRows,nCols] =
  unsafePerformIO $  do
    canvas <- canvasWithImage nCols nRows img
    imageData <- canvasImageData canvas nCols nRows
    undefined
imageToTensor _ _ = Left "TODO: Hack: Only batch-size-one is supported"

resizeImage :: Img -> IO Img
resizeImage (Img base64Bytes) = do
  undefined

canvasWithImage :: Int -> Int -> Img -> IO Canvas.Canvas
canvasWithImage width height img = liftIO $ do
  canvas <- Canvas.create width height
  imgJs <- createImage img
  ctx <- Canvas.getContext canvas
  Canvas.drawImage imgJs 0 0 width height ctx
  return canvas


foreign import javascript unsafe "ctx = $1.getContext('2d'); $r = getImageData(0,0,$2,$3);"
  canvasImageData :: Canvas.Canvas -> Int -> Int -> IO JSVal

foreign import javascript unsafe "$r = document.Image(); $r.src=\"#$1\";"
  createImageWithSrc_ :: JSString -> IO JSVal

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

  
