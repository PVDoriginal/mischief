module Mischief.Render.Image where

import Codec.Picture qualified as P
import Mischief.Assets.Asset
import Mischief.Assets.Image
import Mischief.ECS.Prelude
import Mischief.Render.Core
import Mischief.Render.Texture

newtype ImageTexture = ImageTexture Texture deriving anyclass (Component)

uploadImage :: OnLoad Image -> System ()
uploadImage (OnLoad entity) = do
  undefined