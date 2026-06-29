module MischiefAssets.Image where

import Codec.Picture qualified as P
import Data.ByteString qualified as B
import Data.Either (fromRight)
import MischiefAssets.Asset

newtype Image = Image {inner :: P.DynamicImage}

instance Asset Image where
  loadAsset !path = do
    bytes <- B.readFile path
    pure . Image . fromRight undefined . P.decodeImage $ bytes
  extensions = ["png", "jpg"]

instance Show Image where
  show :: Image -> String
  show img = show $ P.pixelAt (P.convertRGB16 img.inner) 0 0