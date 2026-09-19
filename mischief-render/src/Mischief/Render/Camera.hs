module Mischief.Render.Camera where

import Codec.Picture
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.Default
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Primitive.Ptr (nullPtr)
import Data.Vector.Storable qualified as VS
import Data.Word
import Foreign (with)
import Foreign.C.ConstPtr
import GHC.Generics (Generic)
import Linear (V3 (V3), V4 (V4), (!*!))
import Mischief.ECS.Events
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as S
import Mischief.Math.Transform
import Mischief.Render.Buffer
import Mischief.Render.Core
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
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
  type IsExclusiveRel OutputTo = True

newtype CameraTexture = CameraTexture Texture

instance Component CameraTexture where
  onRemove =
    [ hook $ \(HookContext entity) -> do
        Just (CameraTexture (Texture {texture})) <- get entity [q|CameraTexture|]
        liftIO $ wgpuTextureRelease texture
    ]

newtype CameraMatrices = CameraMatrices (Buffer Matrices) deriving anyclass (Component)

data CameraPlugin

instance Plugin CameraPlugin where
  init = do
    void $ Observers.spawn onAddCameraOutputTo

    systems updateCameraMatrices
      & schedule Update

updateCameraMatrices :: System ()
updateCameraMatrices = do
  device <- res @RenderDevice
  queue <- res @RenderQueue
  for_ ((,) <$> device <*> queue) go
  where
    go :: (RenderDevice, RenderQueue) -> System ()
    go (device, queue) = do
      cameras <- query [q|Entity, Maybe CameraMatrices, Transform / With Camera|]
      for_ cameras $ \(entity, buffer, transform) -> do
        buffer <- case buffer of
          Just (CameraMatrices b) -> pure b
          Nothing -> do
            buffer <- createBuffer @Matrices device
            insert (CameraMatrices buffer) entity
            pure buffer

        uploadBuffer queue buffer (getCameraProjection transform)

onAddCameraOutputTo :: OnAddRel OutputTo -> System ()
onAddCameraOutputTo (OnAddRel entity target) = updateCameraTexture entity target

-- | Creates a new CameraTexture component that fits the targeted window.
updateCameraTexture :: Entity -> Entity -> System ()
updateCameraTexture camera window = do
  Just device <- res @RenderDevice
  window <- get window [q|WindowSize|]
  case window of
    Nothing -> warn "Camera output window not found."
    Just (WindowSize width height) -> do
      texture <- liftIO $ createTexture device (textureDescriptor {Texture.width, height})
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

data Matrices f = Matrices
  { projection :: Field f Mat4x4,
    view :: Field f Mat4x4
  }
  deriving stock (Generic)
  deriving anyclass (Bufferable)

getCameraProjection :: Transform -> Matrices CPU
getCameraProjection Transform {translation = pos} = do
  let l = pos.x - 350.0
  let r = pos.x + 350.0
  let t = pos.y + 250.0
  let b = pos.y - 250.0
  let n = 0
  let f = 100

  let forward = V3 0 0 1
  let right = V3 1 0 0
  let up = V3 0 1 0

  let rotation =
        V4
          (V4 right.x right.y right.z 0)
          (V4 up.x up.y up.z 0)
          (V4 forward.x forward.y forward.z 0)
          (V4 0 0 0 1)

  let translation' =
        V4
          (V4 1 0 0 0)
          (V4 0 1 0 0)
          (V4 0 0 1 0)
          (V4 (-pos.x) (-pos.y) (-pos.z) 1)

  let translation :: V4 (V4 Float) = rotation !*! translation'
  let V4 v1 v2 v3 v4 = translation

  Matrices
    -- projection matrix
    ( V4 (2 / (r - l)) 0 0 (-((r + l) / (r - l))),
      V4 0 (2 / (t - b)) 0 (-((t + b) / (t - b))),
      V4 0 0 (-(2 / (f - n))) (-((f + n) / (f - n))),
      V4 0 0 0 1
    )
    -- view matrix
    ( v1,
      v2,
      v3,
      v4
    )
