-- |


module Grace.Image where

import Codec.Picture (DynamicImage)

data Img = Img { inner :: DynamicImage }
