module Mischief.Render.Sprite where

import Data.Foldable
import GHC.Generics
import Linear.Matrix (M44 (..))
import Linear.V4
import Mischief.Assets (Image (Image))
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.Math
import Mischief.Math.Transform
import Mischief.Render.Buffer
import Mischief.Render.Camera
import Mischief.Render.Core
import Mischief.Render.Material
import Mischief.Render.Plugin (RenderUpdate (RenderUpdate), getFormat, newSampler)
import Mischief.Render.Shader.Bindings (Bindable (..), Binding, Uniform)
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.Functions
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Texture
import Mischief.WGPU.Types.Enums (wGPUTextureFormat_RGBA8Unorm)
import Mischief.WGPU.Types.General (WGPUBindGroupEntry (textureView))

newtype Sprite = Sprite {image :: Entity} deriving anyclass (Component)

data SpritePlugin = SpritePlugin deriving (Eq)

instance Plugin SpritePlugin where
  init _ = do
    Systems.add RenderUpdate renderSprites

newtype ImageTexture = ImageTexture Texture deriving anyclass (Component)

renderSprites :: System ()
renderSprites = do
  sprites <- [q|*Sprite, *Transform|]
  for_ sprites $ \(Sprite {image}, spriteT) -> do
    image <- [g|E, *Image, *Maybe ImageTexture|] image
    for_ image $ \(imageEntity, image, imageTexture) -> do
      cameras <- [q|*CameraTexture, *Transform, OutputTo -> (*RenderSurface, *RenderAdapter, *RenderDevice, *RenderQueue)|]
      for_ cameras $ \(CameraTexture texture, cameraT, (surface, adapter, device, queue)) -> do
        format <- getFormat surface adapter
        let material = Material {vertex, fragment, format = TextureFormat wGPUTextureFormat_RGBA8Unorm, draw = SimpleDraw 6}

        buf <- createBuffer @Matrices device
        let mat = getCameraProjection cameraT
        uploadBuffer queue buf (getCameraProjection cameraT)

        buf' <- createBuffer @SpriteCoords device
        uploadBuffer queue buf' (SpriteCoords spriteT.translation)

        -- let (a, b, c, d) = mat.view
        -- let (a', b', c', d') = mat.projection
        -- let x = (V4 (-10) (-10) 0 1 *! V4 a b c d) *! V4 a' b' c' d'
        -- let y = (V4 30 (-10) 0 1 *! V4 a b c d) *! V4 a' b' c' d'
        -- warn $ text x

        sampler <- newSampler device

        tex <- case imageTexture of
          Just (ImageTexture texture) -> pure texture
          Nothing -> do
            texture <- createTextureForImage device image
            uploadImage queue image texture
            insert (ImageTexture texture) imageEntity
            pure texture

        view <- createView tex

        render device queue Bindings {matrices = buf, spritePos = buf', sampler, texture = view} material texture

data Matrices f = Matrices
  { projection :: Field f Mat4x4,
    view :: Field f Mat4x4
  }
  deriving stock (Generic)
  deriving anyclass (Bufferable)

data VertexOutput f = VertexOutput
  { pos :: BuiltIn f "position" Vec4f,
    uv :: Location f 0 Vec2f
  }
  deriving (Generic, ShaderParam)

data Bindings f = Bindings
  { matrices :: Uniform f 0 (Matrices f),
    spritePos :: Uniform f 1 (SpriteCoords f),
    texture :: Binding f 2 Texture,
    sampler :: Binding f 3 Sampler
  }
  deriving stock (Generic)
  deriving anyclass (Bindable)

newtype SpriteCoords f = SpriteCoords {coords :: Field f Vec3f}
  deriving stock (Generic)
  deriving anyclass (Bufferable)

vertex :: Bindings GPU -> BIn "vertex_index" U32 -> Shader (VertexOutput GPU)
vertex b (BIn index) = do
  positions <- var $ array @6 (vec2f (-1, -1), vec2f (1, -1), vec2f (1, 1), vec2f (-1, -1), vec2 (1, 1), vec2 (-1, 1))
  uvs <- var $ array @6 (vec2f (0, 1), vec2f (1, 1), vec2f (1, 0), vec2f (0, 1), vec2f (1, 0), vec2f (0, 0))

  let pos' = (positions `at` index) * 40 + vec2 (b.spritePos.coords.x, b.spritePos.coords.y)
  let pos = b.matrices.projection ^** (b.matrices.view ^** vec4 (pos'.x, pos'.y, 0, 1))
  pure $
    VertexOutput
      { pos = vec4 (pos.x, pos.y, 0, 1),
        uv = uvs `at` index
      }

fragment :: Bindings GPU -> VertexOutput GPU -> Shader (Loc 0 Vec4f)
fragment b input = pure $ Loc $ sample b.texture b.sampler input.uv

getCameraProjection :: Transform -> Matrices CPU
getCameraProjection Transform {translation = pos} = do
  let l = pos.x - 350.0
  let r = pos.x + 350.0
  let t = pos.y + 250.0
  let b = pos.y - 250.0
  let n = 0
  let f = 100

  let forward = V3 0 0 (-1)
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