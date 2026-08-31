module Mischief.Render.Buffer where

import Control.Monad.IO.Class
import Foreign (Ptr, allocaBytes, castPtr, nullPtr, with, (.|.))
import Foreign.C.ConstPtr (ConstPtr (..))
import Mischief.ECS.Prelude
import Mischief.Render.Core
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.State (CPU)
import Mischief.WGPU (wgpuDeviceCreateBuffer, wgpuQueueWriteBuffer, withWGPUString)
import Mischief.WGPU.Opaque (WGPUBuffer)
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data Buffer a = Buffer {buffer :: Ptr WGPUBuffer, desc :: BufferDescriptor}

data BufferDescriptor = BufferDescriptor
  { label :: String,
    size :: Int,
    usages :: [BufferUsage],
    mappedAtCreation :: Bool
  }

data BufferUsage
  = BufferUsageNone
  | BufferUsageMapRead
  | BufferUsageMapWrite
  | BufferUsageCopySrc
  | BufferUsageCopyDst
  | BufferUsageIndex
  | BufferUsageVertex
  | BufferUsageUniform
  | BufferUsageStorage
  | BufferUsageIndirect
  | BufferUsageQueryResolve

usageBits :: BufferUsage -> WGPUBufferUsage
usageBits = \case
  BufferUsageNone -> wGPUBufferUsage_None
  BufferUsageMapRead -> wGPUBufferUsage_MapRead
  BufferUsageMapWrite -> wGPUBufferUsage_MapWrite
  BufferUsageCopySrc -> wGPUBufferUsage_CopySrc
  BufferUsageCopyDst -> wGPUBufferUsage_CopyDst
  BufferUsageIndex -> wGPUBufferUsage_Index
  BufferUsageVertex -> wGPUBufferUsage_Vertex
  BufferUsageUniform -> wGPUBufferUsage_Uniform
  BufferUsageStorage -> wGPUBufferUsage_Storage
  BufferUsageIndirect -> wGPUBufferUsage_Indirect
  BufferUsageQueryResolve -> wGPUBufferUsage_QueryResolve

processUsages :: [BufferUsage] -> WGPUBufferUsage
processUsages = foldr ((.|.) . usageBits) (WGPUBufferUsage (WGPUFlags 0))

createBuffer' :: RenderDevice -> BufferDescriptor -> System (Buffer a)
createBuffer' (RenderDevice device) descriptor = liftIO $ do
  let BufferDescriptor {label, size, usages, mappedAtCreation} = descriptor

  withWGPUString label $ \label -> do
    let desc =
          WGPUBufferDescriptor
            { nextInChain = nullPtr,
              label,
              usage = processUsages usages,
              size = fromIntegral size,
              mappedAtCreation = if mappedAtCreation then wgpuTrue else wgpuFalse
            }

    buffer <- with desc $ wgpuDeviceCreateBuffer device . ConstPtr
    pure $ Buffer {buffer, desc = descriptor}

createBuffer :: forall a. (Bufferable a) => RenderDevice -> System (Buffer a)
createBuffer device = do
  createBuffer'
    device
    BufferDescriptor
      { label = "buffer",
        size = getSize @a,
        usages = [BufferUsageCopyDst, BufferUsageUniform],
        mappedAtCreation = False
      }

uploadBuffer :: forall a. (Bufferable a) => RenderQueue -> Buffer a -> a CPU -> System ()
uploadBuffer (RenderQueue queue) Buffer {buffer, desc = BufferDescriptor {size}} a = liftIO $ do
  allocaBytes size $ \ptr -> do
    _ <- putBytes a ptr
    wgpuQueueWriteBuffer queue buffer 0 (ConstPtr $ castPtr ptr) (fromIntegral size)
