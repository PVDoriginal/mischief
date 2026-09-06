{-# LANGUAGE GADTSyntax #-}

module Mischief.Render.Material where

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign (nullPtr, with)
import Foreign.C.ConstPtr
import GHC.TypeLits
import Language.Haskell.TH (Extension (GADTSyntax))
import Mischief.ECS.Prelude
import Mischief.Render.Core
import Mischief.Render.Core (TextureFormat)
import Mischief.Render.Shader
import Mischief.Render.Shader.Bindings (Bindable, createBindGroup, createBindLayout)
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.State
import Mischief.Render.Texture
import Mischief.WGPU
import Mischief.WGPU.Framework (loadShaderFromBytes)
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data Material bindings vIn vOut fOut where
  Material ::
    { vertex :: bindings GPU -> vIn GPU -> Shader (vOut GPU),
      fragment :: bindings GPU -> vOut GPU -> Shader (fOut GPU),
      format :: TextureFormat,
      draw :: DrawType
    } ->
    Material bindings vIn vOut fOut

newtype DrawType = SimpleDraw {vertices :: Nat}

createPipeline :: forall bindings vIn vOut fOut. (Bindable bindings, ShaderParam vIn, ShaderParam vOut, ShaderParam fOut) => RenderDevice -> Material bindings vIn vOut fOut -> IO Pipeline
createPipeline (RenderDevice device) mat = do
  BindLayout bindLayout <- createBindLayout @bindings (RenderDevice device)

  pipelineLayoutDesc <- with bindLayout $ \bindGroupLayout -> do
    pure $
      WGPUPipelineLayoutDescriptor
        { nextInChain = nullPtr,
          label = WGPUStringView {_data = ConstPtr nullPtr, length = 0},
          bindGroupLayoutCount = 1,
          bindGroupLayouts = ConstPtr bindGroupLayout,
          immediateSize = 0
        }

  pipelineLayout <- with pipelineLayoutDesc $ \pipelineLayoutDesc -> do
    wgpuDeviceCreatePipelineLayout device (ConstPtr pipelineLayoutDesc)

  vertShader <- loadShaderFromBytes device (T.encodeUtf8 $ genShader @vIn @vOut "vertex" mat.vertex)
  fragShader <- loadShaderFromBytes device (T.encodeUtf8 $ genShader @vOut @fOut "fragment" mat.fragment)

  pipeline <- withWGPUString "main" $ \vertexEntry -> do
    withWGPUString "main" $ \fragmentEntry -> do
      withWGPUString "render pipeline" $ \label -> do
        let multisample =
              WGPUMultisampleState
                { nextInChain = nullPtr,
                  count = 1,
                  mask = 0xFFFFFFFF,
                  alphaToCoverageEnabled = wgpuFalse
                }

        let primitive =
              newWGPUPrimitiveState
                { topology = wGPUPrimitiveTopology_TriangleList
                }
        with blendState $ \blend -> do
          let (TextureFormat format) = mat.format
          let target =
                WGPUColorTargetState
                  { nextInChain = nullPtr,
                    format,
                    blend = ConstPtr blend,
                    writeMask = wGPUColorWriteMask_All
                  }

          let vertex =
                newVertexState
                  { entryPoint = vertexEntry,
                    _module = vertShader,
                    _WGPUVertexState = Proxy
                  }

          with target $ \target -> do
            let fragment =
                  newFragmentState
                    { entryPoint = fragmentEntry,
                      _module = fragShader,
                      _WGPUFragmentState = Proxy,
                      targetCount = 1,
                      targets = ConstPtr target
                    }

            with fragment $ \fragment -> do
              let pipelineDesc =
                    WGPURenderPipelineDescriptor
                      { nextInChain = nullPtr,
                        label,
                        layout = pipelineLayout,
                        vertex,
                        multisample,
                        primitive,
                        fragment = ConstPtr fragment,
                        depthStencil = ConstPtr nullPtr
                      }

              with pipelineDesc $ \desc -> wgpuDeviceCreateRenderPipeline device desc

  pure $ Pipeline pipeline

blendState :: WGPUBlendState
blendState =
  WGPUBlendState
    { color =
        WGPUBlendComponent
          { srcFactor = wGPUBlendFactor_SrcAlpha,
            dstFactor = wGPUBlendFactor_OneMinusSrcAlpha,
            operation = wGPUBlendOperation_Add
          },
      alpha =
        WGPUBlendComponent
          { srcFactor = wGPUBlendFactor_One,
            dstFactor = wGPUBlendFactor_OneMinusSrcAlpha,
            operation = wGPUBlendOperation_Add
          }
    }

render :: forall bindings vIn vOut fOut. (Bindable bindings, ShaderParam vIn, ShaderParam vOut, ShaderParam fOut) => RenderDevice -> RenderQueue -> bindings CPU -> Material bindings vIn vOut fOut -> Texture -> System ()
render (RenderDevice device) (RenderQueue queue) b material (Texture {texture = output}) = liftIO $ do
  -- let b = toLink @bindings cpu

  bindLayout <- createBindLayout @bindings (RenderDevice device)

  bindGroup <- createBindGroup (RenderDevice device) bindLayout b
  Pipeline pipeline <- createPipeline (RenderDevice device) material

  frame <- wgpuTextureCreateView output (ConstPtr nullPtr)

  commandEncoder <- withWGPUString "command encoder" $ \label -> do
    let desc = WGPUCommandEncoderDescriptor {nextInChain = nullPtr, label}
    with desc $ \desc -> wgpuDeviceCreateCommandEncoder device desc

  let attachment =
        WGPURenderPassColorAttachment
          { clearValue = wgpuColor 0 1 0 1,
            storeOp = wGPUStoreOp_Store,
            loadOp = wGPULoadOp_Clear,
            resolveTarget = nullPtr,
            depthSlice = wGPU_DEPTH_SLICE_UNDEFINED,
            view = frame,
            nextInChain = nullPtr
          }

  renderPassEncoder <- with attachment $ \attachment -> do
    withWGPUString "render pass encoder" $ \label -> do
      let desc =
            WGPURenderPassDescriptor
              { timestampWrites = ConstPtr nullPtr,
                occlusionQuerySet = nullPtr,
                depthStencilAttachment = ConstPtr nullPtr,
                colorAttachments = ConstPtr attachment,
                colorAttachmentCount = 1,
                label,
                nextInChain = nullPtr
              }

      with desc $ \desc -> do
        wgpuCommandEncoderBeginRenderPass commandEncoder (ConstPtr desc)

  when (renderPassEncoder == nullPtr) $ error "Couldn't encode render pass."

  wgpuRenderPassEncoderSetPipeline renderPassEncoder pipeline
  wgpuRenderPassEncoderSetBindGroup renderPassEncoder 0 bindGroup 0 (ConstPtr nullPtr)
  wgpuRenderPassEncoderDraw renderPassEncoder (fromIntegral material.draw.vertices) 1 0 0
  wgpuRenderPassEncoderEnd renderPassEncoder
  wgpuRenderPassEncoderRelease renderPassEncoder

  commandBuffer <- withWGPUString "command buffer" $ \label -> do
    let desc = WGPUCommandBufferDescriptor {label, nextInChain = nullPtr}
    with desc $ \desc -> wgpuCommandEncoderFinish commandEncoder (ConstPtr desc)

  when (commandBuffer == nullPtr) $ error "Couldn't create command buffer."

  with commandBuffer $ \commandBuffer -> do
    wgpuQueueSubmit queue 1 (ConstPtr commandBuffer)

  wgpuCommandBufferRelease commandBuffer
  wgpuCommandEncoderRelease commandEncoder
  wgpuTextureViewRelease frame
