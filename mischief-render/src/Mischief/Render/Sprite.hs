{-# OPTIONS_GHC -Wno-partial-fields #-}

module Mischief.Render.Sprite where

import Control.Monad.IO.Class
-- import Mischief.Render.Plugin (RenderUpdate (RenderUpdate), getFormat, newSampler)

import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Traversable
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
import Mischief.Render.Shader.Bindings (Bindable (..), Binding, Uniform)
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.Functions hiding ((&))
import Mischief.Render.Shader.Functions qualified as F hiding ((&))
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.Singletons (PrimitiveTypes (TInt), Types (Primitive))
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Texture
import Mischief.WGPU.Types.Enums (wGPUTextureFormat_RGBA8Unorm)
import Mischief.WGPU.Types.General (WGPUBindGroupEntry (textureView), WGPUExtent3D (height, width))

newtype Sprite = Sprite {image :: Entity}

instance Component Sprite where
  required = require @SpriteFlip

data SpriteSlice = SpriteSlice {start :: V2 Nat, size :: V2 Nat} deriving (Component, Eq)

data SpriteFlip = SpriteFlip {x :: Bool, y :: Bool} deriving (Component)

instance Default SpriteFlip where
  def = SpriteFlip {x = False, y = False}

data SpritePlugin

instance Plugin SpritePlugin where
  init = do
    systems renderSprites
      & schedule RenderUpdate
  deps = [dep @ImageUploadingPlugin]

newtype SpriteBuffer = SpriteBuffer (Buffer SpriteData) deriving anyclass (Component)

renderSprites :: System ()
renderSprites = do
  adapter <- res @RenderAdapter
  device <- res @RenderDevice
  queue <- res @RenderQueue
  let resources = (,,) <$> adapter <*> device <*> queue

  for_ resources $ \(_, device, queue) -> do
    cameras <- query [q|CameraTexture, CameraMatrices|]
    sprites <- query [q|Entity, Sprite, Transform, Maybe SpriteSlice, SpriteFlip, Maybe SpriteBuffer|]
    for_ cameras $ \(CameraTexture texture, CameraMatrices buf) -> do
      commands <- for sprites $ \(sprite, Sprite {image}, spriteT, slice', SpriteFlip {x = flipX, y = flipY}, buffer) -> do
        buffer <- case buffer of
          Just (SpriteBuffer b) -> pure b
          Nothing -> do
            buffer <- createBuffer @SpriteData device
            insert (SpriteBuffer buffer) sprite
            pure buffer

        image <- get image [q|ImageTexture, ImageTextureView|]
        for image $ \(ImageTexture Texture {desc}, ImageTextureView imageView) -> do
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

          let calcFlip x = if x then 1 else 0
          let flip = V2 (calcFlip flipX) (calcFlip flipY)

          uploadBuffer queue buffer (SpriteData {coords = spriteT.translation, size, slice, flip})
          sampler <- newSampler device
          pure $ Draw Bindings {matrices = buf, sprite = buffer, sampler, texture = imageView} 6

      let material = Material {vertex, fragment, format = TextureFormat wGPUTextureFormat_RGBA8Unorm}
      render device queue material texture (catMaybes commands)

renderSprites' :: System ()
renderSprites' =
  query_ $ do
    (cam, CameraTexture texture, CameraMatrices matrices, Res device, Res queue) <- [q|E, CameraTexture, CameraMatrices, Res RenderDevice, Res RenderQueue|]
    [q|Sprite, (Transform, Maybe SpriteSlice, SpriteFlip, Maybe SpriteBuffer)|]
      & qget (\(Sprite {image}, _) -> Just image) [q|ImageTexture, ImageTextureView|]
      & qjoin (\(_, b) image -> (image.comp, b))
      & qtraverse
        ( \sprite ((ImageTexture Texture {desc}, ImageTextureView imageView), (spriteT, slice', SpriteFlip {x = flipX, y = flipY}, buffer)) -> do
            buffer <- case buffer of
              Just (SpriteBuffer b) -> pure b
              Nothing -> do
                buffer <- createBuffer @SpriteData device
                insert (SpriteBuffer buffer) sprite
                pure buffer

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

            let calcFlip x = if x then 1 else 0
            let flip = V2 (calcFlip flipX) (calcFlip flipY)

            uploadBuffer queue buffer (SpriteData {coords = spriteT.translation, size, slice, flip})
            sampler <- newSampler device
            pure $ Draw Bindings {matrices = matrices, sprite = buffer, sampler, texture = imageView} 6
        )
      & qcollect cam
      & qtraverse
        ( \_ commands -> do
            render device queue Material {vertex, fragment, format = TextureFormat wGPUTextureFormat_RGBA8Unorm} texture commands
        )

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
    flip :: Field f Vec2i
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
  color <- sampleSprite b uv
  -- color <- outline b color uv Outline {color = vec4 (1, 0, 0, 1), thickness = 0.03}
  pure $ Loc color

data Outline = Outline {color :: Vec4f, thickness :: F32}

outline :: Bindings GPU -> Vec4f -> Vec2f -> Outline -> Shader Vec4f
outline b color' uv Outline {color, thickness} = do
  let sample offset = sampleSprite b (uv + offset *. thickness)

  left <- sample $ vec2f (-1, 0)
  up <- sample $ vec2f (0, -1)
  right <- sample $ vec2f (1, 0)
  down <- sample $ vec2f (0, 1)

  let colorAlpha = color'.a
  let outlineAlpha = maxAll [left.a, up.a, right.a, down.a] * (1 - colorAlpha)

  pure $ color' *. colorAlpha + color *. outlineAlpha

sampleSprite :: Bindings GPU -> Vec2f -> Shader Vec4f
sampleSprite b uv = do
  let uv_flipped = F.abs $ cast b.sprite.flip - uv
  let uv = uv_flipped * vec2 (b.sprite.slice.z, b.sprite.slice.w) + vec2 (b.sprite.slice.x, b.sprite.slice.y)
  pure $ sample b.texture b.sampler uv
