-- |

{-# LANGUAGE NamedFieldPuns #-}

module Grace.Image where

-- import Codec.Picture (DynamicImage)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Vector as Vec
import qualified Control.Lens as Lens
import qualified Data.Text.Encoding as Text
-- import Debug.Trace (trace, traceShowId, traceShow)
import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import qualified Codec.Picture.Extra as Juicy
import qualified Data.ByteString.Base64 as Base64
import System.IO.Unsafe (unsafePerformIO)

data Img = Img { base64Image :: Text }
  deriving (Eq, Show)

-- TODO: hack: this function assumes the output tensor is channel-major,
-- and we cheat by putting it into a batch of 1. We also assume that
-- the second dimension is the channel count, and only handle 1 channel
-- (monochrome) or 3 channels (rgb). We assume the image is a jpeg.
imageToTensor :: Img -> [Int] -> Either String ((Int, Int), [Float])
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

  pure $ ((nCols, nRows), List.concat channelMajor)
imageToTensor (Img {base64Image}) [-1,-1,-1, 3] = do
  imageBytes <- Base64.decode (Text.encodeUtf8 base64Image)
  image <- fmap Juicy.convertRGB8 $ Juicy.decodeJpeg imageBytes
  let
    pixelValues :: Juicy.PixelRGB8 -> [Float]
    pixelValues (Juicy.PixelRGB8 r g b) =
      let
          r' = realToFrac r / 255.0 / 2.0 + 0.1
          g' = realToFrac g / 255.0 / 2.0 + 0.1
          b' = realToFrac b / 255.0 / 2.0 + 0.1
        in [ r', g', b' ]

    pixelList = fmap pixelValues $ Lens.toListOf Juicy.imagePixels image

  pure $ ((Juicy.imageWidth image, Juicy.imageHeight image), List.concat pixelList)
imageToTensor _ _ = Left "TODO: HACK: Only batch-size-one is supported"

imageFromTensor :: [Int] -> [Float] -> Img
imageFromTensor [_,height,width, 3] elements =
  let
    values = Vec.fromList elements
    juicyImage = unsafePerformIO $ Juicy.withImage width height (\r c ->
      let
        offset = c * 3 + r * width * 3
        red = floor $ (values   Vec.! offset) * 255.0
        green = floor $ (values Vec.! offset + 1) * 255.0
        blue = floor $ (values  Vec.! offset + 2) * 255.0
      in pure $ Juicy.PixelRGB8 red green blue)
    imgBytes = Text.decodeUtf8 $ Base64.encode . ByteString.toStrict . Juicy.encodeJpeg $ Juicy.convertImage juicyImage
  in
    Img imgBytes
