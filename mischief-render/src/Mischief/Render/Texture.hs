module Mischief.Render.Texture where

import Codec.Picture qualified as P
import Codec.Picture.Extra (scaleBilinear)
import Control.Applicative.Singletons
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.Maybe.Singletons (SMaybe (SJust))
import Data.Singletons
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as VS
import Foreign (Bits ((.|.)), Ptr, Word32, Word8, castPtr, nullPtr, with)
import Foreign.C.ConstPtr (ConstPtr (..))
import GHC.TypeLits
import Mischief.Assets.Image
import Mischief.ECS
import Mischief.Render.Core
import Mischief.WGPU
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data TextureDescriptor = TextureDescriptor
  { width :: Int,
    height :: Int,
    depth :: Int,
    label :: String,
    mipLevels :: Int,
    samples :: Int,
    format :: WGPUTextureFormat,
    usages :: [TextureUsage]
  }
  deriving (Show)

instance Default TextureDescriptor where
  def = textureDescriptor

textureDescriptor :: TextureDescriptor
textureDescriptor =
  TextureDescriptor
    { width = 1,
      height = 1,
      depth = 1,
      label = "texture",
      mipLevels = 1,
      samples = 1,
      format = wGPUTextureFormat_RGBA8Unorm,
      usages = [TextureUsageTextureBinding, TextureUsageCopyDst]
    }

data TextureUsage
  = TextureUsageNone
  | TextureUsageCopySrc
  | TextureUsageCopyDst
  | TextureUsageTextureBinding
  | TextureUsageStorageBinding
  | TextureUsageRenderAttachment
  | TextureUsageTransientAttachment
  deriving (Show)

usageBits :: TextureUsage -> WGPUTextureUsage
usageBits TextureUsageNone = wGPUTextureUsage_None
usageBits TextureUsageCopySrc = wGPUTextureUsage_CopySrc
usageBits TextureUsageCopyDst = wGPUTextureUsage_CopyDst
usageBits TextureUsageTextureBinding = wGPUTextureUsage_TextureBinding
usageBits TextureUsageStorageBinding = wGPUTextureUsage_StorageBinding
usageBits TextureUsageRenderAttachment = wGPUTextureUsage_RenderAttachment
usageBits TextureUsageTransientAttachment = wGPUTextureUsage_TransientAttachment

processUsages :: [TextureUsage] -> WGPUTextureUsage
processUsages = foldr ((.|.) . usageBits) (WGPUTextureUsage (WGPUFlags 0))

createTexture :: RenderDevice -> TextureDescriptor -> System Texture
createTexture (RenderDevice device) descriptor = liftIO $ do
  let TextureDescriptor {width, height, depth, label, mipLevels, samples, format, usages} = descriptor
  withWGPUString label $ \label -> do
    let desc =
          WGPUTextureDescriptor
            { size = WGPUExtent3D (fromIntegral width) (fromIntegral height) (fromIntegral depth),
              label,
              mipLevelCount = fromIntegral mipLevels,
              sampleCount = fromIntegral samples,
              dimension = wGPUTextureDimension_2D,
              format,
              usage = processUsages usages,
              nextInChain = nullPtr,
              viewFormatCount = 0,
              viewFormats = ConstPtr nullPtr
            }

    (`Texture` descriptor) <$> with desc (wgpuDeviceCreateTexture device . ConstPtr)

getDimension :: (Int, Int, Int) -> WGPUTextureDimension
getDimension (_, 1, 1) = wGPUTextureDimension_1D
getDimension (_, _, 1) = wGPUTextureDimension_2D
getDimension (_, _, _) = wGPUTextureDimension_3D

uploadImage :: RenderQueue -> Image -> Texture -> System ()
uploadImage (RenderQueue queue) (Image image) (Texture {texture, desc = TextureDescriptor {width, height}}) = liftIO $ do
  let (P.Image w h bytes) = scaleBilinear width height (P.convertRGBA8 image)

  let extent = WGPUExtent3D (fromIntegral w) (fromIntegral h) 1
  let copyInfo =
        WGPUTexelCopyTextureInfo
          { texture,
            mipLevel = 0,
            origin = WGPUOrigin3D 0 0 0,
            aspect = wGPUTextureAspect_All
          }

  let layout =
        WGPUTexelCopyBufferLayout
          { offset = 0,
            bytesPerRow = 4 * fromIntegral w,
            rowsPerImage = fromIntegral h
          }

  VS.unsafeWith bytes $ \pixelPtr -> do
    with extent $ \extent -> do
      with copyInfo $ \copyInfo -> do
        with layout $ \layout -> do
          wgpuQueueWriteTexture queue (ConstPtr copyInfo) (ConstPtr $ castPtr pixelPtr) (fromIntegral $ VS.length bytes) (ConstPtr layout) (ConstPtr extent)

data Texture = Texture {texture :: Ptr WGPUTexture, desc :: TextureDescriptor}