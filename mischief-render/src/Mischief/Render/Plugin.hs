module Mischief.Render.Plugin where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable (for_)
import Data.Traversable
import Foreign (Storable (peek), free, malloc, nullPtr, with)
import Foreign.C.ConstPtr (ConstPtr (..))
import GHC.Generics
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude hiding (get, set)
import Mischief.ECS.Systems qualified as Systems
import Mischief.Render.Bind
import Mischief.Render.Camera
import Mischief.Render.Core
import Mischief.Render.Material
import Mischief.Render.Shader.Bindings hiding (Bindings)
import Mischief.Render.Shader.Functions (sample)
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.SDL (SDLPlugin (..))
import Mischief.SDL.Window
import Mischief.WGPU
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data RenderPlugin = RenderPlugin deriving (Eq)

instance Plugin RenderPlugin where
  init _ = do
    insertRes =<< liftIO (RenderInstance <$> wgpuCreateInstance)
    void $ Observers.spawn onAddWindow
    void $ Observers.spawn onAddCameraOutputTo
    Systems.add Update renderCameras

  plugins _ = plug SDLPlugin

renderCameras :: System ()
renderCameras = do
  cameras <- [q|*CameraTexture, OutputTo -> (*RenderSurface, *RenderAdapter, *RenderDevice, *RenderQueue)|]
  for_ cameras $ \(CameraTexture camera, (surface, adapter, device, queue)) -> do
    format <- getFormat surface adapter
    withSurfaceTexture surface $ \output -> do
      let material = Material {vertex, fragment, format}

      sampler <- bindSampler =<< newSampler device
      tex <- bindTexture camera

      render device queue Bindings {tex, sampler} material output
      presentSurface surface

data VertexOutput = VertexOutput
  { pos :: BuiltIn "position" Vec4f,
    uv :: Location 0 Vec2f
  }
  deriving (Generic, Default, ShaderParam)

vertex :: Bindings -> BuiltIn "vertex_index" U32 -> Shader VertexOutput
vertex _ index = do
  positions <- var $ array @3 @(VectorType VL2 TFloat) (vec2 (-1, -1), vec2 (3, -1), vec2 (-1, 3))
  let pos = positions `at` get index
  pure $
    VertexOutput
      { pos = set $ vec4 (pos.x, pos.y, 0, 1),
        uv = set $ vec2 (pos.x * 0.5 + 0.5, 1.0 - (pos.y * 0.5 + 0.5))
      }

data Bindings = Bindings
  { tex :: Binding 0 Texture2d,
    sampler :: Binding 1 Sampler
  }
  deriving (Generic, Default, Bindable)

fragment :: Bindings -> VertexOutput -> Shader (Location 0 Vec4f)
fragment b input = pure $ set $ sample (get b.tex) (get b.sampler) (get input.uv)

type FullScreenMaterial = Material Bindings (BuiltIn "vertex_index" U32) VertexOutput (Location 0 Vec4f)

getFormat :: RenderSurface -> RenderAdapter -> System TextureFormat
getFormat (RenderSurface surface) (RenderAdapter adapter) = liftIO $ do
  surfaceCapabilities <- malloc @WGPUSurfaceCapabilities
  wgpuSurfaceGetCapabilities surface adapter surfaceCapabilities

  cap <- peek surfaceCapabilities
  free surfaceCapabilities
  let (ConstPtr formats) = cap.formats
  format <- peek formats

  pure $ TextureFormat format

withSurfaceTexture :: RenderSurface -> (Texture -> System a) -> System a
withSurfaceTexture (RenderSurface surface) f = do
  surfaceTextureBox <- liftIO malloc
  liftIO $ wgpuSurfaceGetCurrentTexture surface surfaceTextureBox
  surfaceTexture <- liftIO $ peek surfaceTextureBox

  res <- f (Texture surfaceTexture.texture)

  liftIO $ free surfaceTextureBox
  return res

newSampler :: RenderDevice -> System TextureSampler
newSampler (RenderDevice device) = liftIO $ do
  let samplerDesc =
        WGPUSamplerDescriptor
          { addressModeU = wGPUAddressMode_ClampToEdge,
            addressModeV = wGPUAddressMode_ClampToEdge,
            addressModeW = wGPUAddressMode_ClampToEdge,
            magFilter = wGPUFilterMode_Linear,
            minFilter = wGPUFilterMode_Linear,
            mipmapFilter = wGPUMipmapFilterMode_Nearest,
            nextInChain = nullPtr,
            label = WGPUStringView (ConstPtr nullPtr) 0,
            lodMinClamp = 0,
            lodMaxClamp = 32,
            compare = wGPUCompareFunction_Undefined,
            maxAnisotropy = 1
          }
  TextureSampler <$> with samplerDesc (wgpuDeviceCreateSampler device . ConstPtr)

presentSurface :: RenderSurface -> System ()
presentSurface (RenderSurface surface) = liftIO $ wgpuSurfacePresent surface