module Mischief.WGPU.Framework where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign (with)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Foreign.Ptr
import Mischief.WGPU
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

loadShaderModule :: Ptr WGPUDevice -> String -> IO (Ptr WGPUShaderModule)
loadShaderModule device path = do
  bytes <- BS.readFile path
  loadShaderFromBytes device bytes

loadShaderFromBytes :: Ptr WGPUDevice -> ByteString -> IO (Ptr WGPUShaderModule)
loadShaderFromBytes device bytes = bsAsWGPUString bytes $ \shaderString -> do
  let shaderSource =
        WGPUShaderSourceWGSL
          { chain = WGPUChainedStruct {sType = wGPUSType_ShaderSourceWGSL, next = nullPtr},
            code = shaderString
          }

  with shaderSource $ \shaderSource -> do
    let desc =
          WGPUShaderModuleDescriptor
            { label = WGPUStringView {_data = ConstPtr nullPtr, length = 0},
              nextInChain = castPtr shaderSource
            }

    with desc $ \desc -> do
      wgpuDeviceCreateShaderModule device (ConstPtr desc)
