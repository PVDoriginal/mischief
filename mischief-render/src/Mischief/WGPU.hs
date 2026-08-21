{-# OPTIONS_GHC -Wno-orphans #-}

module Mischief.WGPU where

import Foreign
import Foreign.C.ConstPtr
import Foreign.C.Types
import Mischief.WGPU.Enums
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types

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

newWGPURequestAdapterCallbackInfo :: WGPURequestAdapterCallbackInfo
newWGPURequestAdapterCallbackInfo =
  WGPURequestAdapterCallbackInfo
    { nextInChain = nullPtr,
      mode = wGPUCallbackMode_AllowSpontaneous,
      callback = nullFunPtr,
      userdata1 = nullPtr,
      userdata2 = nullPtr
    }

foreign import ccall "wrapper.h hs_wgpuInstanceRequestAdapter" wgpuInstanceRequestAdapter :: Ptr WGPUInstance -> Ptr WGPURequestAdapterOptions -> Ptr WGPURequestAdapterCallbackInfo -> IO ()
