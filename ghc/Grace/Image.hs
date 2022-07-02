-- |

{-# LANGUAGE NamedFieldPuns #-}

module Grace.Image where

-- import Codec.Picture (DynamicImage)
import qualified Data.List as List
import Data.Text (Text)
import qualified Control.Lens as Lens
import qualified Data.Text.Encoding as Text
-- import Debug.Trace (trace, traceShowId, traceShow)
import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Extra as Juicy
import qualified Data.ByteString.Base64 as Base64

data Img = Img { base64Image :: Text }
  deriving (Eq, Show)

-- TODO: hack: this function assumes the output tensor is channel-major,
-- and we cheat by putting it into a batch of 1. We also assume that
-- the second dimension is the channel count, and only handle 1 channel
-- (monochrome) or 3 channels (rgb). We assume the image is a jpeg.
imageToTensor :: Img -> [Int] -> Either String [Float]
imageToTensor (Img {base64Image}) [1,nChans,nRows,nCols] = do
  imageBytes <- Base64.decode (Text.encodeUtf8 base64Image)
  image <- fmap Juicy.convertRGB8 $ Juicy.decodeJpeg imageBytes
  let scaledImage =
        if Juicy.imageWidth image == nCols && Juicy.imageHeight image == nRows
        then image
        else Juicy.scaleBilinear nCols nRows image
  let
    pixelValues :: Juicy.PixelRGB8 -> [Float]
    pixelValues (Juicy.PixelRGB8 r g b) = case nChans of
      1 -> let
        v = (realToFrac r + realToFrac g + realToFrac b) / 255.0 / 3.0 in [v]
      3 ->
        let
          r' = realToFrac r / 255.0
          g' = realToFrac g / 255.0
          b' = realToFrac b / 255.0
        in [ r', g', b' ]
      _ -> error "TODO: HACK: Only 1 channel or 3 channel images supported."
    pixelList = fmap pixelValues $ Lens.toListOf Juicy.imagePixels (scaledImage)
    channelMajor = List.transpose pixelList

  pure $ List.concat channelMajor
imageToTensor _ _ = Left "TODO: HACK: Only batch-size-one is supported"
