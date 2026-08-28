module Mischief.Assets.Image where

import Codec.Picture qualified as P
import Data.ByteString qualified as B
import Data.Either (fromRight)
import Mischief.Assets.Asset
import Mischief.ECS.Prelude

newtype Image = Image P.DynamicImage deriving anyclass (Component)

instance Asset Image where
  loadAsset !bytes = do
    pure . Image . fromRight undefined . P.decodeImage $ bytes
