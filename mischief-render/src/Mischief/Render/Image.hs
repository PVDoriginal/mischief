module Mischief.Render.Image where

import Codec.Picture qualified as P
import Control.Monad
import Data.Foldable
import Data.Primitive.Ptr
import Foreign.C.ConstPtr
import Mischief.Assets.Asset
import Mischief.Assets.Image
import Mischief.ECS.Observer
import Mischief.ECS.Observers qualified as Observer
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as S
import Mischief.Render.Core
import Mischief.Render.Texture
import Mischief.WGPU
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

newtype ImageTexture = ImageTexture Texture deriving anyclass (Component)

newtype ImageTextureView = ImageTextureView TextureView deriving anyclass (Component)

data ImageUploadingPlugin = ImageUploadingPlugin deriving (Eq)

data QueueUpload = QueueUpload deriving (Component)

instance Plugin ImageUploadingPlugin where
  init _ = do
    S.add Update uploadImages
    enableImageUploadOnLoad

uploadImages :: System ()
uploadImages = do
  images <- [q|Entity, *Image / With QueueUpload|]
  for_ images $ \(e, _) -> remove (C @QueueUpload) e

  device <- res @RenderDevice
  queue <- res @RenderQueue

  for_ ((,) <$> device <*> queue) $ \(device, queue) -> do
    for_ images $ \(entity, image) -> do
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

queueUploadOnLoad :: OnLoad Image -> System ()
queueUploadOnLoad (OnLoad e) = insert QueueUpload e

data ImageObserver = ImageObserver deriving (Component)

enableImageUploadOnLoad :: System ()
enableImageUploadOnLoad = void $ spawn (Observer queueUploadOnLoad, ImageObserver)

disableImageUploadOnLoad :: System ()
disableImageUploadOnLoad = [s|Entity / With ImageObserver|] >>= despawn . unwrap