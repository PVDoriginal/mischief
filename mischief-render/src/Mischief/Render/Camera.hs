module Mischief.Render.Camera where

import Codec.Picture
import Control.Monad.IO.Class
import Data.Bits
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Primitive.Ptr (nullPtr)
import Data.Vector.Storable qualified as VS
import Data.Word
import Foreign (with)
import Foreign.C.ConstPtr
import GHC.Generics (Generic)
import Mischief.ECS.Events
import Mischief.ECS.Prelude
import Mischief.Render.Core
import Mischief.Render.Texture
import Mischief.Render.Texture (createTexture)
import Mischief.Render.Texture qualified as Texture
import Mischief.SDL.Window
import Mischief.WGPU (wgpuDeviceCreateTexture, wgpuTextureRelease, withWGPUString)
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data Camera = Camera deriving (Component)

-- | Add this as a relationship between a camera and a window to have the
-- camera's output texture be rendered to that window.
--
-- Exclusive.
data OutputTo = OutputTo

instance Component OutputTo where
  type RelExclusivity OutputTo = Exclusive

newtype CameraTexture = CameraTexture Texture

instance Component CameraTexture where
  onRemove =
    [ hook $ \(HookContext entity) -> do
        Just (CameraTexture (Texture {texture})) <- [g|*CameraTexture|] entity
        liftIO $ wgpuTextureRelease texture
    ]

onAddCameraOutputTo :: OnAddRel OutputTo -> System ()
onAddCameraOutputTo (OnAddRel entity target) = updateCameraTexture entity target

-- | Creates a new CameraTexture component that fits the targeted window.
updateCameraTexture :: Entity -> Entity -> System ()
updateCameraTexture camera window = do
  window <- [g|*WindowSize, *RenderDevice|] window
  case window of
    Nothing -> warn "Camera output window not found."
    Just (WindowSize width height, device) -> do
      texture <- createTexture device (textureDescriptor {Texture.width, height})
      insert (CameraTexture texture) camera
      info "Created new camera texture!"

newTextureDescriptor :: Int -> Int -> WGPUStringView -> WGPUTextureDescriptor
newTextureDescriptor w h label =
  WGPUTextureDescriptor
    { size = WGPUExtent3D (fromIntegral w) (fromIntegral h) 1,
      label,
      mipLevelCount = 1,
      sampleCount = 1,
      dimension = wGPUTextureDimension_2D,
      format = wGPUTextureFormat_RGBA8Unorm,
      usage = wGPUTextureUsage_TextureBinding .|. wGPUTextureUsage_CopyDst,
      nextInChain = nullPtr,
      viewFormatCount = 0,
      viewFormats = ConstPtr nullPtr
    }
