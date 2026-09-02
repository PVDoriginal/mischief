module Mischief.Render.Sprite where

import Data.Foldable
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.Math.Transform
import Mischief.Render.Camera
import Mischief.Render.Core
import Mischief.Render.Plugin (RenderUpdate (RenderUpdate))
import Mischief.Render.Texture

newtype Sprite = Sprite {image :: Entity} deriving anyclass (Component)

data SpritePlugin = SpritePlugin deriving (Eq)

instance Plugin SpritePlugin where
  init _ = do
    Systems.add RenderUpdate renderSprites

renderSprites :: System ()
renderSprites = do
  sprites <- [q|*Sprite, *Transform|]
  cameras <- [q|*CameraTexture, OutputTo -> (*RenderSurface, *RenderAdapter, *RenderDevice, *RenderQueue)|]
  for_ cameras $ \(CameraTexture Texture {texture}, (surface, adapter, device, queue)) -> do
    undefined