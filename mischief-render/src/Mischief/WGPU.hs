{-# OPTIONS_GHC -Wno-orphans #-}

module Mischief.WGPU where

import Data.ByteString
import Data.ByteString qualified as BS
import Data.ByteString.Char8 as BS8
import Data.Data (Proxy (Proxy))
import Data.Void
import Foreign
import Foreign.C.ConstPtr
import Foreign.C.Types
import Mischief.WGPU.Callbacks
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

foreign import ccall "webgpu.h wgpuCreateInstance" c_wgpuCreateInstance :: Ptr () -> IO (Ptr WGPUInstance)

wgpuCreateInstance :: IO (Ptr WGPUInstance)
wgpuCreateInstance = c_wgpuCreateInstance nullPtr

foreign import ccall "webgpu.h wgpuInstanceCreateSurface" wgpuInstanceCreateSurface :: Ptr WGPUInstance -> ConstPtr WGPUSurfaceDescriptor -> IO (Ptr WGPUSurface)

newWGPURequestAdapterOptions :: WGPURequestAdapterOptions
newWGPURequestAdapterOptions =
  WGPURequestAdapterOptions
    { nextInChain = nullPtr,
      featureLevel = wGPUFeatureLevel_Core,
      powerPreference = wGPUPowerPreference_Undefined,
      forceFallbackAdapter = wgpuFalse,
      backendType = wGPUBackendType_Undefined,
      compatibleSurface = nullPtr
    }

newWGPURequestCallbackInfo :: WGPURequestCallbackInfo a
newWGPURequestCallbackInfo =
  WGPURequestCallbackInfo
    { nextInChain = nullPtr,
      mode = wGPUCallbackMode_WaitAnyOnly,
      callback = nullFunPtr,
      userdata1 = nullPtr,
      userdata2 = nullPtr
    }

foreign import ccall "wrapper.h wgpuInstanceProcessEvents" wgpuInstanceProcessEvents :: Ptr WGPUInstance -> IO ()

foreign import ccall "wrapper.h hs_wgpuInstanceRequestAdapter" wgpuInstanceRequestAdapter :: Ptr WGPUInstance -> Ptr WGPUSurface -> IO (Ptr WGPUAdapter)

foreign import ccall "wrapper.h hs_wgpuAdapterRequestDevice" wgpuAdapterRequestDevice :: Ptr WGPUAdapter -> IO (Ptr WGPUDevice)

foreign import ccall "webgpu.h wgpuDeviceGetQueue" wgpuDeviceGetQueue :: Ptr WGPUDevice -> IO (Ptr WGPUQueue)

foreign import ccall "webgpu.h wgpuDeviceCreateShaderModule" wgpuDeviceCreateShaderModule :: Ptr WGPUDevice -> ConstPtr WGPUShaderModuleDescriptor -> IO (Ptr WGPUShaderModule)

newWGPUPipelineLayoutDescriptor :: WGPUPipelineLayoutDescriptor
newWGPUPipelineLayoutDescriptor =
  WGPUPipelineLayoutDescriptor
    { nextInChain = nullPtr,
      label = WGPUStringView {_data = ConstPtr nullPtr, length = 0},
      bindGroupLayoutCount = 0,
      bindGroupLayouts = ConstPtr nullPtr,
      immediateSize = 0
    }

bsAsWGPUString :: ByteString -> (WGPUStringView -> IO a) -> IO a
bsAsWGPUString bs f = BS.useAsCStringLen bs $ \(ptr, length) -> f WGPUStringView {_data = ConstPtr ptr, length}

foreign import ccall "webgpu.h wgpuDeviceCreatePipelineLayout" wgpuDeviceCreatePipelineLayout :: Ptr WGPUDevice -> ConstPtr WGPUPipelineLayoutDescriptor -> IO (Ptr WGPUPipelineLayout)

foreign import ccall "webgpu.h wgpuSurfaceGetCapabilities" wgpuSurfaceGetCapabilities :: Ptr WGPUSurface -> Ptr WGPUAdapter -> Ptr WGPUSurfaceCapabilities -> IO ()

withWGPUString :: String -> (WGPUStringView -> IO a) -> IO a
withWGPUString s = bsAsWGPUString (BS8.pack s)

foreign import ccall "webgpu.h wgpuDeviceCreateRenderPipeline" wgpuDeviceCreateRenderPipeline :: Ptr WGPUDevice -> Ptr WGPURenderPipelineDescriptor -> IO (Ptr WGPURenderPipeline)

newVertexState :: WGPUVertexState
newVertexState =
  WGPUVertexState
    { nextInChain = nullPtr,
      _module = nullPtr,
      entryPoint = WGPUStringView {_data = ConstPtr nullPtr, length = 0},
      constantCount = 0,
      constants = ConstPtr nullPtr,
      bufferCount = 0,
      buffers = ConstPtr nullPtr,
      _WGPUVertexState = Proxy
    }

newFragmentState :: WGPUFragmentState
newFragmentState =
  WGPUFragmentState
    { nextInChain = nullPtr,
      _module = nullPtr,
      entryPoint = WGPUStringView {_data = ConstPtr nullPtr, length = 0},
      constantCount = 0,
      constants = ConstPtr nullPtr,
      targetCount = 0,
      targets = ConstPtr nullPtr,
      _WGPUFragmentState = Proxy
    }

newWGPUPrimitiveState :: WGPUPrimitiveState
newWGPUPrimitiveState =
  WGPUPrimitiveState
    { nextInChain = nullPtr,
      topology = wGPUPrimitiveTopology_TriangleList,
      stripIndexFormat = wGPUIndexFormat_Undefined,
      frontFace = wGPUFrontFace_CCW,
      cullMode = wGPUCullMode_Undefined,
      unclippedDepth = wgpuFalse
    }

newWGPUSurfaceConfiguration :: WGPUSurfaceConfiguration
newWGPUSurfaceConfiguration =
  WGPUSurfaceConfiguration
    { nextInChain = nullPtr,
      device = nullPtr,
      format = wGPUTextureFormat_Undefined,
      usage = wGPUTextureUsage_RenderAttachment,
      width = 0,
      height = 0,
      viewFormatCount = 0,
      viewFormats = ConstPtr nullPtr,
      alphaMode = wGPUCompositeAlphaMode_Auto,
      presentMode = wGPUPresentMode_Undefined
    }

foreign import ccall "webgpu.h wgpuSurfaceConfigure" wgpuSurfaceConfigure :: Ptr WGPUSurface -> ConstPtr WGPUSurfaceConfiguration -> IO ()

foreign import ccall "webgpu.h wgpuSurfaceGetCurrentTexture" wgpuSurfaceGetCurrentTexture :: Ptr WGPUSurface -> Ptr WGPUSurfaceTexture -> IO ()

foreign import ccall "webgpu.h wgpuTextureCreateView" wgpuTextureCreateView :: Ptr WGPUTexture -> ConstPtr WGPUTextureViewDescriptor -> IO (Ptr WGPUTextureView)

foreign import ccall "webgpu.h wgpuDeviceCreateCommandEncoder" wgpuDeviceCreateCommandEncoder :: Ptr WGPUDevice -> Ptr WGPUCommandEncoderDescriptor -> IO (Ptr WGPUCommandEncoder)

foreign import ccall "webgpu.h wgpuCommandEncoderBeginRenderPass" wgpuCommandEncoderBeginRenderPass :: Ptr WGPUCommandEncoder -> ConstPtr WGPURenderPassDescriptor -> IO (Ptr WGPURenderPassEncoder)

foreign import ccall "webgpu.h wgpuRenderPassEncoderSetPipeline" wgpuRenderPassEncoderSetPipeline :: Ptr WGPURenderPassEncoder -> Ptr WGPURenderPipeline -> IO ()

foreign import ccall "webgpu.h wgpuRenderPassEncoderDraw" wgpuRenderPassEncoderDraw :: Ptr WGPURenderPassEncoder -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall "webgpu.h wgpuRenderPassEncoderEnd" wgpuRenderPassEncoderEnd :: Ptr WGPURenderPassEncoder -> IO ()

foreign import ccall "webgpu.h wgpuCommandEncoderFinish" wgpuCommandEncoderFinish :: Ptr WGPUCommandEncoder -> ConstPtr WGPUCommandBufferDescriptor -> IO (Ptr WGPUCommandBuffer)

foreign import ccall "webgpu.h wgpuQueueSubmit" wgpuQueueSubmit :: Ptr WGPUQueue -> CSize -> ConstPtr (Ptr WGPUCommandBuffer) -> IO ()

foreign import ccall "webgpu.h wgpuRenderPassEncoderRelease" wgpuRenderPassEncoderRelease :: Ptr WGPURenderPassEncoder -> IO ()

foreign import ccall "webgpu.h wgpuRenderPipelineRelease" wgpuRenderPipelineRelease :: Ptr WGPURenderPipeline -> IO ()

foreign import ccall "webgpu.h wgpuPipelineLayoutRelease" wgpuPipelineLayoutRelease :: Ptr WGPUPipelineLayout -> IO ()

foreign import ccall "webgpu.h wgpuSurfaceCapabilitiesFreeMembers" wgpuSurfaceCapabilitiesFreeMembers :: Ptr WGPUSurfaceCapabilities -> IO ()

foreign import ccall "webgpu.h wgpuSurfacePresent" wgpuSurfacePresent :: Ptr WGPUSurface -> IO ()

foreign import ccall "webgpu.h wgpuQueueRelease" wgpuQueueRelease :: Ptr WGPUQueue -> IO ()

foreign import ccall "webgpu.h wgpuDeviceRelease" wgpuDeviceRelease :: Ptr WGPUDevice -> IO ()

foreign import ccall "webgpu.h wgpuAdapterRelease" wgpuAdapterRelease :: Ptr WGPUAdapter -> IO ()

foreign import ccall "webgpu.h wgpuSurfaceRelease" wgpuSurfaceRelease :: Ptr WGPUSurface -> IO ()

foreign import ccall "webgpu.h wgpuShaderModuleRelease" wgpuShaderModuleRelease :: Ptr WGPUShaderModule -> IO ()

foreign import ccall "webgpu.h wgpuInstanceRelease" wgpuInstanceRelease :: Ptr WGPUInstance -> IO ()

foreign import ccall "webgpu.h wgpuCommandBufferRelease" wgpuCommandBufferRelease :: Ptr WGPUCommandBuffer -> IO ()

foreign import ccall "webgpu.h wgpuCommandEncoderRelease" wgpuCommandEncoderRelease :: Ptr WGPUCommandEncoder -> IO ()

foreign import ccall "webgpu.h wgpuTextureViewRelease" wgpuTextureViewRelease :: Ptr WGPUTextureView -> IO ()

foreign import ccall "webgpu.h wgpuTextureRelease" wgpuTextureRelease :: Ptr WGPUTexture -> IO ()

wgpuColor :: Double -> Double -> Double -> Double -> WGPUColor
wgpuColor r g b a = WGPUColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

foreign import ccall safe "webgpu.h wgpuQueueWriteTexture" wgpuQueueWriteTexture :: Ptr WGPUQueue -> ConstPtr WGPUTexelCopyTextureInfo -> ConstPtr Void -> CSize -> ConstPtr WGPUTexelCopyBufferLayout -> ConstPtr WGPUExtent3D -> IO ()

foreign import ccall safe "webgpu.h wgpuDeviceCreateTexture" wgpuDeviceCreateTexture :: Ptr WGPUDevice -> ConstPtr WGPUTextureDescriptor -> IO (Ptr WGPUTexture)

foreign import ccall safe "webgpu.h wgpuDeviceCreateSampler" wgpuDeviceCreateSampler :: Ptr WGPUDevice -> ConstPtr WGPUSamplerDescriptor -> IO (Ptr WGPUSampler)

foreign import ccall "webgpu.h wgpuDeviceCreateBindGroup" wgpuDeviceCreateBindGroup :: Ptr WGPUDevice -> ConstPtr WGPUBindGroupDescriptor -> IO (Ptr WGPUBindGroup)

unusedBufferLayout :: WGPUBufferBindingLayout
unusedBufferLayout =
  WGPUBufferBindingLayout
    { minBindingSize = 0,
      hasDynamicOffset = wgpuFalse,
      _type = wGPUBufferBindingType_BindingNotUsed,
      nextInChain = nullPtr
    }

unusedSamplerLayout :: WGPUSamplerBindingLayout
unusedSamplerLayout =
  WGPUSamplerBindingLayout
    { _type = wGPUSamplerBindingType_BindingNotUsed,
      nextInChain = nullPtr
    }

unusedTextureLayout :: WGPUTextureBindingLayout
unusedTextureLayout =
  WGPUTextureBindingLayout
    { sampleType = wGPUTextureSampleType_BindingNotUsed,
      multisampled = wgpuFalse,
      viewDimension = wGPUTextureViewDimension_1D,
      nextInChain = nullPtr
    }

unusedStorageTextureLayout :: WGPUStorageTextureBindingLayout
unusedStorageTextureLayout =
  WGPUStorageTextureBindingLayout
    { viewDimension = wGPUTextureViewDimension_1D,
      format = wGPUTextureFormat_ASTC10x10Unorm,
      access = wGPUStorageTextureAccess_BindingNotUsed,
      nextInChain = nullPtr
    }

foreign import ccall "webgpu.h wgpuDeviceCreateBindGroupLayout" wgpuDeviceCreateBindGroupLayout :: Ptr WGPUDevice -> ConstPtr WGPUBindGroupLayoutDescriptor -> IO (Ptr WGPUBindGroupLayout)

foreign import ccall "webgpu.h wgpuRenderPassEncoderSetBindGroup" wgpuRenderPassEncoderSetBindGroup :: Ptr WGPURenderPassEncoder -> Word32 -> Ptr WGPUBindGroup -> CSize -> ConstPtr Word32 -> IO ()

foreign import ccall "webgpu.h wgpuDeviceCreateBuffer" wgpuDeviceCreateBuffer :: Ptr WGPUDevice -> ConstPtr WGPUBufferDescriptor -> IO (Ptr WGPUBuffer)

foreign import ccall "webgpu.h wgpuQueueWriteBuffer" wgpuQueueWriteBuffer :: Ptr WGPUQueue -> Ptr WGPUBuffer -> Word64 -> ConstPtr Void -> CSize -> IO ()
