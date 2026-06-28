module MischiefRender where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign (Ptr, Storable (pokeByteOff), Word8, castPtr, plusPtr)
import Foreign.C
import GPUCommon
import MischiefECS
import SDL3

renderPlugin :: Plugin ()
renderPlugin = do
  addSystems Startup setup

setup :: System ()
setup = do
  liftIO $ sdlSetLogPriorities SDL_LOG_PRIORITY_VERBOSE
  Just window <- single @Window
  Just device <- liftIO $ sdlCreateGPUDevice SDL_GPU_SHADERFORMAT_SPIRV True Nothing
  b <- liftIO $ sdlClaimWindowForGPUDevice device window.sdlWindow

  vertexBytes <- liftIO $ B.readFile "assets/test_vert.spv"
  fragmentBytes <- liftIO $ B.readFile "assets/test_frag.spv"

  liftIO $ unsafeUseAsCStringLen vertexBytes $ \b -> do
    unsafeUseAsCStringLen fragmentBytes $ \b' -> do
      runPass b b' device window.sdlWindow
    return ()

runPass :: (Ptr CChar, Int) -> (Ptr CChar, Int) -> SDLGPUDevice -> SDLWindow -> IO ()
runPass (vertexBytes, vertexLen) (fragmentBytes, fragmentLen) device window = do
  Just vertex <-
    sdlCreateGPUShader
      device
      SDLGPUShaderCreateInfo
        { shaderCode = castPtr vertexBytes,
          shaderCodeSize = fromIntegral vertexLen,
          shaderEntryPoint = "vertex",
          shaderFormat = SDL_GPU_SHADERFORMAT_SPIRV,
          shaderStage = SDL_GPU_SHADERSTAGE_VERTEX,
          shaderNumSamplers = 0,
          shaderNumStorageTextures = 0,
          shaderNumStorageBuffers = 0,
          shaderNumUniformBuffers = 0,
          shaderProps = 0
        }

  Just fragment <-
    sdlCreateGPUShader
      device
      SDLGPUShaderCreateInfo
        { shaderCode = castPtr fragmentBytes,
          shaderCodeSize = fromIntegral fragmentLen,
          shaderEntryPoint = "vertex",
          shaderFormat = SDL_GPU_SHADERFORMAT_SPIRV,
          shaderStage = SDL_GPU_SHADERSTAGE_VERTEX,
          shaderNumSamplers = 0,
          shaderNumStorageTextures = 0,
          shaderNumStorageBuffers = 0,
          shaderNumUniformBuffers = 0,
          shaderProps = 0
        }

  Just commands <- sdlAcquireGPUCommandBuffer device

  Just (tex, width, height) <-
    sdlWaitAndAcquireGPUSwapchainTexture
      commands
      window

  print width
  print height

  Just transfer <-
    sdlCreateGPUTransferBuffer
      device
      SDLGPUTransferBufferCreateInfo
        { transferUsage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
          transferSize = 4 * width * height,
          transferProps = 0
        }

  Just ptr <- sdlMapGPUTransferBuffer device transfer False

  forM_ [0 .. width * height - 1] $ \i -> do
    let p = ptr `plusPtr` (fromIntegral i * 4)
    pokeByteOff p 0 (0 :: Word8) -- B
    pokeByteOff p 1 (0 :: Word8) -- G
    pokeByteOff p 2 (255 :: Word8) -- R
    pokeByteOff p 3 (255 :: Word8) -- A
  Just copyPass <- sdlBeginGPUCopyPass commands

  sdlUploadToGPUTexture
    copyPass
    ( SDLGPUTextureTransferInfo
        transfer
        0
        width
        height
    )
    ( SDLGPUTextureRegion
        tex
        0
        0
        0
        0
        0
        width
        height
        1
    )
    False

  sdlEndGPUCopyPass copyPass

  b <- sdlSubmitGPUCommandBuffer commands
  print b
  return ()