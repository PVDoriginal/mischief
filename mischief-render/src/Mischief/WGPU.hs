{-# OPTIONS_GHC -Wno-orphans #-}

module Mischief.WGPU where

import Data.ByteString
import Data.ByteString qualified as BS
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
      forceFallbackAdapter = WGPUBool False,
      backendType = wGPUBackendType_Undefined,
      compatibleSurface = nullPtr
    }

newWGPURequestCallbackInfo :: WGPURequestCallbackInfo a
newWGPURequestCallbackInfo =
  WGPURequestCallbackInfo
    { nextInChain = nullPtr,
      mode = wGPUCallbackMode_AllowSpontaneous,
      callback = nullFunPtr,
      userdata1 = nullPtr,
      userdata2 = nullPtr
    }

foreign import ccall "wrapper.h hs_wgpuInstanceRequestAdapter" wgpuInstanceRequestAdapter :: Ptr WGPUInstance -> Ptr WGPURequestAdapterOptions -> Ptr (WGPURequestCallbackInfo WGPURequestAdapterCallback) -> IO ()

foreign import ccall "wrapper.h hs_wgpuAdapterRequestDevice" wgpuAdapterRequestDevice :: Ptr WGPUAdapter -> Ptr () -> Ptr (WGPURequestCallbackInfo WGPURequestDeviceCallback) -> IO ()

foreign import ccall "webgpu.h wgpuDeviceGetQueue" wgpuDeviceGetQueue :: Ptr WGPUDevice -> IO (Ptr WGPUQueue)

foreign import ccall "webgpu.h wgpuDeviceCreateShaderModule" wgpuDeviceCreateShaderModule :: Ptr WGPUDevice -> Ptr WGPUShaderModuleDescriptor -> IO (Ptr WGPUShaderModule)

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

foreign import ccall "webgpu.h wgpuDeviceCreatePipelineLayout" wgpuDeviceCreatePipelineLayout :: Ptr WGPUDevice -> Ptr WGPUPipelineLayoutDescriptor -> IO (Ptr WGPUPipelineLayout)

foreign import ccall "webgpu.h wgpuSurfaceGetCapabilities" wgpuSurfaceGetCapabilities :: Ptr WGPUSurface -> Ptr WGPUAdapter -> Ptr WGPUSurfaceCapabilities -> IO ()