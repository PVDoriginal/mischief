module Mischief.Render.Image where

import Codec.Picture qualified as P
import Data.Primitive.Ptr
import Foreign
import Foreign.C.ConstPtr
import Mischief.Assets.Asset
import Mischief.Assets.Image
import Mischief.ECS.Observers qualified as Obs
import Mischief.ECS.Prelude
import Mischief.Render.Core
import Mischief.Render.Texture
import Mischief.WGPU
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

newtype ImageTexture = ImageTexture Texture deriving anyclass (Component)

newtype ImageTextureView = ImageTextureView TextureView deriving anyclass (Component)

data ImageUploadingPlugin = ImageUploadingPlugin deriving (Eq)

instance Plugin ImageUploadingPlugin where
  init _ = do
    _ <- Obs.spawn onImageLoad
    pure ()

onImageLoad :: OnLoad Image -> System ()
onImageLoad (OnLoad entity) = do
  Just image <- [g|*Image|] entity
  Just device <- res @RenderDevice
  Just queue <- res @RenderQueue
  runAfter
    ( do
        tex <- createTextureForImage device image
        uploadImage queue image tex
        view <- TextureView <$> wgpuTextureCreateView tex.texture (ConstPtr nullPtr)

        pure (tex, view)
    )
    ( \(tex, view) -> do
        insert (ImageTexture tex, ImageTextureView view) entity
    )