-- |


module Grace.Image where

-- import Codec.Picture (DynamicImage)
import Data.Text (Text)

data Img = Img { base64Image :: Text }
  deriving (Eq, Show)
