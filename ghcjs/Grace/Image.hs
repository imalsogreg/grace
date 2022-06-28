-- | 

module Grace.Image where

import JavaScript.Web.Canvas
import Data.Text (Text)

data Img = Img { base64Image :: Text }
  deriving (Eq, Show)

