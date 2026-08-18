module Mischief.WGPU where

import Foreign.C.Types

foreign import ccall "webgpu.h wgpuCommandBufferSetLabel" c_test :: CInt -> CInt -> IO ()
