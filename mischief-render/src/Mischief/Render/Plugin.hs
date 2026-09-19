module Mischief.Render.Plugin where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable (for_)
import Data.Traversable
import Foreign (Storable (peek), free, malloc, nullPtr, with)
import Foreign.C.ConstPtr (ConstPtr (..))
import GHC.Generics
import Mischief.ECS (Before, Schedule, UpdateSchedule (..), scheduleEntity)
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude hiding (get, set)
import Mischief.ECS.Relationships.Order (Before (..))
import Mischief.ECS.Systems qualified as Systems
import Mischief.Render.Bind
import Mischief.Render.Buffer
import Mischief.Render.Camera
import Mischief.Render.Core
import Mischief.Render.Material
import Mischief.Render.Shader.Bindings hiding (Bindings)
import Mischief.Render.Shader.Buffers (Bufferable, Field)
import Mischief.Render.Shader.Functions (sample)
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.Singletons
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Sprite (SpritePlugin (..))
import Mischief.Render.Texture
import Mischief.SDL (SDLPlugin (..))
import Mischief.SDL.Window
import Mischief.WGPU
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data RenderPlugin = RenderPlugin deriving (Eq)

instance Plugin RenderPlugin where
  init = do
    first <- scheduleEntity RenderFirst
    update <- scheduleEntity RenderUpdate
    last <- scheduleEntity RenderLast

    update' <- scheduleEntity PostUpdate

    insert (Rel Before update) first
    insert (Rel Before last) update
    insert (Rel Before first) update'

    for_ [first, update, last] $ insert UpdateSchedule

    insertRes =<< liftIO (RenderInstance <$> wgpuCreateInstance)
    void $ Observers.spawn onAddWindow

    systems renderCameras
      & schedule RenderLast

  deps = [dep @SDLPlugin, dep @SpritePlugin, dep @CameraPlugin]

renderCameras :: System ()
renderCameras = do
  resources <- getRenderingResources
  case resources of
    Nothing -> pure ()
    Just (adapter, device, queue) -> do
      cameras <- query [q|CameraTexture, OutputTo -> (RenderSurface)|]
      for_ cameras $ \(CameraTexture Texture {texture}, From _ surface) -> do
        format <- getFormat surface adapter
        withSurfaceTexture surface $ \output -> do
          let material = Material {vertex, fragment, format}

          sampler <- newSampler device
          tex <- liftIO $ wgpuTextureCreateView texture (ConstPtr nullPtr)

          let command = Draw Bindings {tex = TextureView tex, sampler} 3
          render device queue material output [command]
          presentSurface surface

data VertexOutput f = VertexOutput
  { pos :: BuiltIn f "position" Vec4f,
    uv :: Location f 0 Vec2f
  }
  deriving (Generic, ShaderParam)

vertex :: Bindings GPU -> BIn "vertex_index" U32 -> Shader (VertexOutput GPU)
vertex _ (BIn index) = do
  positions <- var $ array @3 (vec2f (-1, -1), vec2f (3, -1), vec2f (-1, 3))
  let pos = positions `at` index
  pure $
    VertexOutput
      { pos = vec4 (pos.x, pos.y, 0, 1),
        uv = vec2 (pos.x * 0.5 + 0.5, 1.0 - (pos.y * 0.5 + 0.5))
      }

data Bindings f = Bindings
  { tex :: Binding f 0 Texture,
    sampler :: Binding f 1 Sampler
  }
  deriving (Generic, Bindable)

fragment :: Bindings GPU -> VertexOutput GPU -> Shader (Loc 0 Vec4f)
fragment b input = do
  pure $ Loc $ sample b.tex b.sampler input.uv

-- fragment b input = pure $ Loc $ vec4 (b.coord.x, b.coord.y, 0, 1)

-- type FullScreenMaterial = Material Bindings (BuiltIn "vertex_index" U32) VertexOutput (Location 0 Vec4f)

withSurfaceTexture :: RenderSurface -> (Texture -> System a) -> System a
withSurfaceTexture (RenderSurface surface) f = do
  surfaceTextureBox <- liftIO malloc
  liftIO $ wgpuSurfaceGetCurrentTexture surface surfaceTextureBox
  surfaceTexture <- liftIO $ peek surfaceTextureBox

  res <- f (Texture {texture = surfaceTexture.texture, desc = def})

  liftIO $ free surfaceTextureBox
  return res

presentSurface :: RenderSurface -> System ()
presentSurface (RenderSurface surface) = liftIO $ wgpuSurfacePresent surface