module Mischief.WGPU where

import Foreign
import Foreign.C.Types
import Mischief.WGPU.Types (WGPUSType)

data WGPUInstance = WGPUInstance

foreign import ccall "webgpu.h wgpuCreateInstance" c_wgpuCreateInstance :: Ptr WGPUInstance -> IO (Ptr WGPUInstance)

wgpuCreateInstance :: IO (Ptr WGPUInstance)
wgpuCreateInstance = c_wgpuCreateInstance nullPtr

data WGPUSurface = WGPUSurface

data WGPUAdapter = WGPUAdapter

data WGPUDevice = WGPUDevice

data WGPUSurfaceConfiguration = WGPUSurfaceConfiguration