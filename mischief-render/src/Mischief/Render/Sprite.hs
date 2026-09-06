{-# OPTIONS_GHC -Wno-partial-fields #-}

module Mischief.Render.Sprite where

import Control.Monad.IO.Class
import Data.Foldable
import GHC.Generics
import GHC.TypeLits
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
import Mischief.Render.Image
import Mischief.Render.Material
import Mischief.Render.Plugin (RenderUpdate (RenderUpdate), getFormat, newSampler)
import Mischief.Render.Shader.Bindings (Bindable (..), Binding, Uniform)
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.Functions
import Mischief.Render.Shader.Functions qualified as F
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.Singletons (PrimitiveTypes (TInt), Types (Primitive))
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Texture
import Mischief.WGPU.Types.Enums (wGPUTextureFormat_RGBA8Unorm)
import Mischief.WGPU.Types.General (WGPUBindGroupEntry (textureView), WGPUExtent3D (height, width))

newtype Sprite = Sprite {image :: Entity} deriving anyclass (Component)

data SpriteSlice = SpriteSlice {start :: V2 Nat, size :: V2 Nat} deriving (Component, Eq)

data SpriteFlipX = SpriteFlipX deriving (Component, Eq)

data SpritePlugin = SpritePlugin deriving (Eq)

instance Plugin SpritePlugin where
  init _ = do
    Systems.add RenderUpdate renderSprites
  plugins _ = plug ImageUploadingPlugin

renderSprites :: System ()
renderSprites = do
  resources <- getRenderingResources
  for_ resources $ \(adapter, device, queue) -> do
    sprites <- [q|*Sprite, *Transform, *Maybe SpriteSlice, *Maybe SpriteFlipX|]
    for_ sprites $ \(Sprite {image}, spriteT, slice', flipX) -> do
      image <- [g|*ImageTexture, *ImageTextureView|] image
      for_ image $ \(ImageTexture Texture {desc}, ImageTextureView imageView) -> do
        cameras <- [q|*CameraTexture, *Transform, OutputTo -> (*RenderSurface)|]
        for_ cameras $ \(CameraTexture texture, cameraT, surface) -> do
          format <- getFormat surface adapter
          let material = Material {vertex, fragment, format = TextureFormat wGPUTextureFormat_RGBA8Unorm, draw = SimpleDraw 6}

          buf <- createBuffer @Matrices device
          let mat = getCameraProjection cameraT
          uploadBuffer queue buf (getCameraProjection cameraT)

          buf' <- createBuffer @SpriteData device

          let size = case slice' of
                Just SpriteSlice {size = V2 x y} -> V2 (fromIntegral x) (fromIntegral y)
                Nothing -> V2 (fromIntegral desc.width) (fromIntegral desc.height)

          let slice = case slice' of
                Just SpriteSlice {start, size} ->
                  V4
                    (fromIntegral start.x / fromIntegral desc.width)
                    (fromIntegral start.y / fromIntegral desc.height)
                    (fromIntegral size.x / fromIntegral desc.width)
                    (fromIntegral size.y / fromIntegral desc.height)
                Nothing -> V4 0 0 1 1

          let flip = case flipX of
                Nothing -> 0
                Just _ -> 1

          uploadBuffer queue buf' (SpriteData {coords = spriteT.translation, size, slice, flipX = flip})

          sampler <- newSampler device

          render device queue Bindings {matrices = buf, sprite = buf', sampler, texture = imageView} material texture

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
    sprite :: Uniform f 1 (SpriteData f),
    texture :: Binding f 2 Texture,
    sampler :: Binding f 3 Sampler
  }
  deriving stock (Generic)
  deriving anyclass (Bindable)

data SpriteData f = SpriteData
  { coords :: Field f Vec3f,
    size :: Field f Vec2f,
    slice :: Field f Vec4f,
    flipX :: Field f I32
  }
  deriving stock (Generic)
  deriving anyclass (Bufferable)

vertex :: Bindings GPU -> BIn "vertex_index" U32 -> Shader (VertexOutput GPU)
vertex b (BIn index) = do
  positions <- var $ array @6 (vec2f (-1, -1), vec2f (1, -1), vec2f (1, 1), vec2f (-1, -1), vec2 (1, 1), vec2 (-1, 1))
  uvs <- var $ array @6 (vec2f (0, 1), vec2f (1, 1), vec2f (1, 0), vec2f (0, 1), vec2f (1, 0), vec2f (0, 0))

  pos' <- var $ (positions `at` index) * b.sprite.size * 0.5
  pos'' <- var $ vec3 (pos'.x, pos'.y, 0) + b.sprite.coords
  pos <- var $ b.matrices.projection *. (b.matrices.view *. vec4 (pos''.x, pos''.y, pos''.z, 1))

  pure
    VertexOutput
      { pos = vec4 (pos.x, pos.y, pos.z, 1),
        uv = uvs `at` index
      }

fragment :: Bindings GPU -> VertexOutput GPU -> Shader (Loc 0 Vec4f)
fragment b VertexOutput {uv} = do
  let thickness = 0.05
  let outline = vec4f (0, 1, 1, 1)

  let sample offset = sampleSprite b (uv + offset * thickness)

  color <- sample $ vec2 (0, 0)

  left <- sample $ vec2 (-1, 0)
  up <- sample $ vec2 (0, -1)
  right <- sample $ vec2 (1, 0)
  down <- sample $ vec2 (0, 1)

  let colorAlpha = color.a
  let outlineAlpha = maxAll [left.a, up.a, right.a, down.a] * (1 - colorAlpha)

  pure . Loc $ color *. colorAlpha + outline *. outlineAlpha

sampleSprite :: Bindings GPU -> Vec2f -> Shader Vec4f
sampleSprite b uv = do
  let uv_flipped = vec2f (cast b.sprite.flipX, 0) - uv
  let uv = F.abs uv_flipped * vec2 (b.sprite.slice.z, b.sprite.slice.w) + vec2 (b.sprite.slice.x, b.sprite.slice.y)
  pure $ sample b.texture b.sampler uv

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
