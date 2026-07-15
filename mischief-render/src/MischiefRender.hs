module MischiefRender where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign (Ptr, Storable (pokeByteOff), Word8, castPtr, plusPtr, pokeArray)
import Foreign.C
import GHC.IO.Handle
import GPUCommon
import Mischief.ECS
import Mischief.ECS.Components.Spawn
import MischiefAssets (registerAsset)
import MischiefRender.Shader (FragmentShader (FragmentShader), VertexShader)
import SDL3
import System.IO.Temp
import System.Process

shadercross :: String
shadercross = "/home/pvd/SDL_shadercross/build/shadercross"

renderPlugin :: Plugin ()
renderPlugin = do
  registerAsset @FragmentShader
  registerAsset @VertexShader
  addSystems Startup setup

setup :: System ()
setup = do
  liftIO $ sdlSetLogPriorities SDL_LOG_PRIORITY_VERBOSE

  m <- entityOf @Window
  Just x <- get @ComponentArchetypes m
  liftIO $ print x

  Just window <- single @Window
  Just device <- liftIO $ sdlCreateGPUDevice SDL_GPU_SHADERFORMAT_SPIRV True Nothing
  b <- liftIO $ sdlClaimWindowForGPUDevice device window.sdlWindow

  vertexBytes <- liftIO $ loadVertex "assets/test.vert.hlsl"
  fragmentBytes <- liftIO $ loadFragment "assets/test.frag.hlsl"

  liftIO $ unsafeUseAsCStringLen vertexBytes $ \b -> do
    unsafeUseAsCStringLen fragmentBytes $ \b' -> do
      runPass b b' device window.sdlWindow
    return ()

loadVertex :: FilePath -> IO B.ByteString
loadVertex path = do
  withSystemTempFile "vert.spv" $ \path' h -> do
    callProcess
      shadercross
      [path, "-e", "main", "-t", "vertex", "-o", path']

    hClose h
    B.readFile path'

loadFragment :: FilePath -> IO B.ByteString
loadFragment path = do
  withSystemTempFile "frag.spv" $ \path' h -> do
    callProcess
      shadercross
      [path, "-e", "main", "-t", "fragment", "-o", path']

    hClose h
    B.readFile path'

runPass :: (Ptr CChar, Int) -> (Ptr CChar, Int) -> SDLGPUDevice -> SDLWindow -> IO ()
runPass (vertexBytes, vertexLen) (fragmentBytes, fragmentLen) device window = do
  Just vertex <-
    sdlCreateGPUShader
      device
      SDLGPUShaderCreateInfo
        { shaderCode = castPtr vertexBytes,
          shaderCodeSize = fromIntegral vertexLen,
          shaderEntryPoint = "main",
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
          shaderEntryPoint = "main",
          shaderFormat = SDL_GPU_SHADERFORMAT_SPIRV,
          shaderStage = SDL_GPU_SHADERSTAGE_FRAGMENT,
          shaderNumSamplers = 0,
          shaderNumStorageTextures = 0,
          shaderNumStorageBuffers = 1,
          shaderNumUniformBuffers = 0,
          shaderProps = 0
        }

  Just commands <- sdlAcquireGPUCommandBuffer device

  Just storageTransfer <-
    sdlCreateGPUTransferBuffer
      device
      SDLGPUTransferBufferCreateInfo
        { transferUsage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
          transferSize = 4 * 4,
          transferProps = 0
        }

  Just storageBuffer <-
    sdlCreateGPUBuffer
      device
      SDLGPUBufferCreateInfo
        { bufferUsage = SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ,
          bufferSize = 4 * 4,
          bufferProps = 0
        }

  Just ptr <- sdlMapGPUTransferBuffer device storageTransfer False
  let color = [0, 0, 1, 1]
  let ptrF = castPtr ptr :: Ptr Float
  pokeArray ptrF color

  sdlUnmapGPUTransferBuffer device storageTransfer
  Just copyCmd <- sdlBeginGPUCopyPass commands

  sdlUploadToGPUBuffer
    copyCmd
    (SDLGPUTransferBufferLocation storageTransfer 0)
    (SDLGPUBufferRegion storageBuffer 0 16)
    False

  sdlEndGPUCopyPass copyCmd

  Just (tex, width, height) <-
    sdlWaitAndAcquireGPUSwapchainTexture
      commands
      window

  print width
  print height

  let vertices =
        concat
          [ [-5, 1, 0, 1],
            [1, 1, 0, 1],
            [1, -5, 0, 1]
          ]

  Just transfer <-
    sdlCreateGPUTransferBuffer
      device
      SDLGPUTransferBufferCreateInfo
        { transferUsage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
          transferSize = 3 * 4 * 4,
          transferProps = 0
        }

  Just vbo <-
    sdlCreateGPUBuffer
      device
      SDLGPUBufferCreateInfo
        { bufferUsage = SDL_GPU_BUFFERUSAGE_VERTEX,
          bufferSize = 3 * 4 * 4,
          bufferProps = 0
        }

  Just ptr <- sdlMapGPUTransferBuffer device transfer False

  let ptrF = castPtr ptr :: Ptr Float

  pokeArray ptrF vertices

  sdlUnmapGPUTransferBuffer device transfer

  Just copyCmd <- sdlBeginGPUCopyPass commands

  sdlUploadToGPUBuffer
    copyCmd
    (SDLGPUTransferBufferLocation transfer 0)
    (SDLGPUBufferRegion vbo 0 48)
    False

  sdlEndGPUCopyPass copyCmd

  Just pipeline <-
    sdlCreateGPUGraphicsPipeline
      device
      SDLGPUGraphicsPipelineCreateInfo
        { vertexShader = vertex,
          fragmentShader = fragment,
          vertexInputState =
            SDLGPUVertexInputState
              { inputVertexBuffers =
                  [ SDLGPUVertexBufferDescription
                      { descSlot = 0,
                        descPitch = 16, -- sizeof(float4)
                        descInputRate = SDL_GPU_VERTEXINPUTRATE_VERTEX,
                        descInstanceStepRate = 0
                      }
                  ],
                inputVertexAttribs =
                  [ SDLGPUVertexAttribute
                      { attribLocation = 0,
                        attribSlot = 0,
                        attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4,
                        attribOffset = 0
                      }
                  ]
              },
          targetInfo = defaultGraphicsPipelineTargetInfo SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM,
          rasterizerState = defaultRasterizerState,
          primitiveType = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
          pipelineProps = 0,
          multisampleState = defaultMultiSampleState,
          depthStencilState = defaultDepthStencilState
        }

  Just pass <-
    sdlBeginGPURenderPass
      commands
      [ SDLGPUColorTargetInfo
          { texture = tex,
            mipLevel = 0,
            layerOrDepthPlane = 0,
            clearColor = SDLFColor 0 0 0 1,
            loadOp = SDL_GPU_LOADOP_CLEAR,
            storeOp = SDL_GPU_STOREOP_STORE,
            resolveTexture = Nothing,
            resolveMipLevel = 0,
            resolveLayer = 0,
            targetCycle = False,
            targetCycleResolve = False
          }
      ]
      Nothing

  sdlBindGPUGraphicsPipeline pass pipeline

  sdlBindGPUVertexBuffers pass 0 [SDLGPUBufferBinding {bindingBuffer = vbo, bindingOffset = 0}]
  sdlBindGPUFragmentStorageBuffers pass 0 [storageBuffer]
  sdlDrawGPUPrimitives pass 3 1 0 0

  sdlEndGPURenderPass pass
  b <- sdlSubmitGPUCommandBuffer commands
  print b
  return ()