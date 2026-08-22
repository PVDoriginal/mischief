import Control.Concurrent
import Control.Monad (forever, unless, when)
import Data.ByteString qualified as BS
import Data.Data (Proxy (..))
import Foreign (Ptr, Storable (peek, poke), alloca, castPtr, free, malloc, nullPtr, with)
import Foreign.C
import Foreign.C.ConstPtr
import Mischief.ECS.Prelude
import Mischief.WGPU
import Mischief.WGPU.Callbacks
import Mischief.WGPU.Framework
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General
import SDL3.Sys (getWindowSize)
import SDL3.Sys qualified as SDL3
import SDL3.Sys.Bindgen.Video.FunPtr (sDL_GetWindowSize)
import System.Environment (setEnv)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  !wgpuInstance <- wgpuCreateInstance

  window <- withCString "sdl3-raw" $ \title -> SDL3.createWindow (ConstPtr title) 640 360 0

  windowProps <- SDL3.getWindowProperties window

  x <- unConstPtr <$> SDL3.getCurrentVideoDriver
  driverName <- peekCString x

  surface <- case driverName of
    "x11" -> do
      display <- BS.useAsCString SDL3.sDL_PROP_WINDOW_X11_DISPLAY_POINTER $ \b -> SDL3.getPointerProperty windowProps (ConstPtr b) nullPtr
      window <- BS.useAsCString SDL3.sDL_PROP_WINDOW_X11_WINDOW_NUMBER $ \b -> SDL3.getNumberProperty windowProps (ConstPtr b) (-1)

      when (display == nullPtr || window == -1) $ error "Can't obtain x11 window."

      let chain = WGPUChainedStruct {next = nullPtr, sType = wGPUSType_SurfaceSourceXlibWindow}
      let xlib = WGPUSurfaceSourceXlibWindow {chain, display, window}

      with xlib $ \xlib -> do
        let desc =
              WGPUSurfaceDescriptor
                { nextInChain = castPtr xlib,
                  label = WGPUStringView {_data = ConstPtr nullPtr, length = 0}
                }
        with desc $ \desc -> do
          wgpuSurface <-
            wgpuInstanceCreateSurface wgpuInstance (ConstPtr desc)

          when (wgpuSurface == nullPtr) $ error "Couldn't create WGPU surface."

          pure wgpuSurface
    "windows" -> do
      hwnd <- BS.useAsCString SDL3.sDL_PROP_WINDOW_WIN32_HWND_POINTER $ \b -> SDL3.getPointerProperty windowProps (ConstPtr b) nullPtr
      hinstance <- BS.useAsCString SDL3.sDL_PROP_WINDOW_WIN32_INSTANCE_POINTER $ \b -> SDL3.getPointerProperty windowProps (ConstPtr b) nullPtr

      let chain = WGPUChainedStruct {next = nullPtr, sType = wGPUSType_SurfaceSourceWindowsHWND}
      let win = WGPUSurfaceSourceWindowsHWND {chain, hinstance, hwnd}

      with win $ \win -> do
        let desc =
              WGPUSurfaceDescriptor
                { nextInChain = castPtr win,
                  label = WGPUStringView {_data = ConstPtr nullPtr, length = 0}
                }

        with desc $ \desc -> do
          wgpuSurface <-
            wgpuInstanceCreateSurface wgpuInstance (ConstPtr desc)

          when (wgpuSurface == nullPtr) $ error "Couldn't create WGPU surface."

          pure wgpuSurface
    _ -> error "undefined video driver"

  -- adapter <- alloca @(Ptr WGPUAdapter) $ \adapterBox -> do
  --   with False $ \b -> do
  --     callback <- requestAdapterCallback onAdapterRequestCall
  --     let callbackInfo =
  --           newWGPURequestCallbackInfo
  --             { callback,
  --               userdata1 = castPtr adapterBox,
  --               userdata2 = castPtr b
  --             }

  --     let adapterOptions = newWGPURequestAdapterOptions {compatibleSurface = surface}
  --     with adapterOptions $ \adapterOptions -> do
  --       with callbackInfo $ \callbackInfo -> do
  --         wgpuInstanceProcessEvents wgpuInstance
  --         wgpuInstanceRequestAdapter wgpuInstance adapterOptions callbackInfo
  --         waitOnBool b

  --         peek adapterBox

  adapter <- wgpuInstanceRequestAdapter wgpuInstance surface

  when (adapter == nullPtr) $ error "Couldn't obtain WGPU adapter."

  -- device <- alloca @(Ptr WGPUDevice) $ \deviceBox -> do
  --   with False $ \b -> do
  --     callback <- requestDeviceCallback onDeviceRequestCall
  --     let callbackInfo =
  --           newWGPURequestCallbackInfo
  --             { callback,
  --               userdata1 = castPtr deviceBox,
  --               userdata2 = castPtr b
  --             }

  --     with callbackInfo $ \callbackInfo -> do
  --       wgpuAdapterRequestDevice adapter nullPtr callbackInfo
  --       waitOnBool b

  --       peek deviceBox

  device <- wgpuAdapterRequestDevice adapter

  when (device == nullPtr) $ error "Couldn't obtain WGPU edvice."

  queue <- wgpuDeviceGetQueue device
  when (queue == nullPtr) $ error "Couldn't obtain WGPU queue."

  shaderModule <- loadShaderModule device "test/shader.wgsl"
  when (shaderModule == nullPtr) $ error "Couldn't load shader module."

  let pipelineLayoutDesc = newWGPUPipelineLayoutDescriptor
  pipelineLayout <- with pipelineLayoutDesc $ \pipelineLayoutDesc -> do
    wgpuDeviceCreatePipelineLayout device pipelineLayoutDesc

  when (pipelineLayout == nullPtr) $ error "Couldn't generate pipeline layout."

  surfaceCapabilities <- malloc @WGPUSurfaceCapabilities
  wgpuSurfaceGetCapabilities surface adapter surfaceCapabilities

  cap <- peek surfaceCapabilities
  free surfaceCapabilities
  let (ConstPtr formats) = cap.formats
  let (ConstPtr alphaModes) = cap.alphaModes
  format <- peek formats
  alphaMode <- peek alphaModes

  pipeline <- withWGPUString "vs_main" $ \vertexEntry -> do
    withWGPUString "fs_main" $ \fragmentEntry -> do
      withWGPUString "render pipeline" $ \label -> do
        let multisample =
              WGPUMultisampleState
                { nextInChain = nullPtr,
                  count = 1,
                  mask = 0xFFFFFFFF,
                  alphaToCoverageEnabled = WGPUBool False
                }

        let primitive =
              newWGPUPrimitiveState
                { topology = wGPUPrimitiveTopology_TriangleList
                }

        let target =
              WGPUColorTargetState
                { nextInChain = nullPtr,
                  format,
                  blend = ConstPtr nullPtr,
                  writeMask = wGPUColorWriteMask_All
                }

        let vertex =
              newVertexState
                { entryPoint = vertexEntry,
                  _module = shaderModule,
                  _WGPUVertexState = Proxy
                }

        with target $ \target -> do
          let fragment =
                newFragmentState
                  { entryPoint = fragmentEntry,
                    _module = shaderModule,
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

  when (pipeline == nullPtr) $ error "Can't create pipeline."

  w <- malloc
  h <- malloc

  _ <- getWindowSize window w h
  width <- peek w
  height <- peek h

  free w
  free h

  let config =
        newWGPUSurfaceConfiguration
          { device,
            usage = wGPUTextureUsage_RenderAttachment,
            format,
            presentMode = wGPUPresentMode_Fifo,
            alphaMode,
            width,
            height
          }

  with config $ \config -> do
    wgpuSurfaceConfigure surface (ConstPtr config)

  _ <- forever $ do
    with cap $ \cap ->
      handleEvents pipeline pipelineLayout shaderModule cap queue device adapter surface wgpuInstance

    surfaceTextureBox <- malloc
    wgpuSurfaceGetCurrentTexture surface surfaceTextureBox
    surfaceTexture <- peek surfaceTextureBox
    free surfaceTextureBox

    frame <- wgpuTextureCreateView surfaceTexture.texture nullPtr
    when (frame == nullPtr) $ error "Couldn't create texture view."

    commandEncoder <- withWGPUString "command encoder" $ \label -> do
      let desc = WGPUCommandEncoderDescriptor {nextInChain = nullPtr, label}
      with desc $ \desc -> wgpuDeviceCreateCommandEncoder device desc

    when (commandEncoder == nullPtr) $ error "Couldn't create command encoder."

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
    wgpuRenderPassEncoderDraw renderPassEncoder 3 1 0 0
    wgpuRenderPassEncoderEnd renderPassEncoder
    wgpuRenderPassEncoderRelease renderPassEncoder

    commandBuffer <- withWGPUString "command buffer" $ \label -> do
      let desc = WGPUCommandBufferDescriptor {label, nextInChain = nullPtr}
      with desc $ \desc -> wgpuCommandEncoderFinish commandEncoder (ConstPtr desc)

    when (commandBuffer == nullPtr) $ error "Couldn't create command buffer."

    with commandBuffer $ \commandBuffer -> do
      wgpuQueueSubmit queue 1 (ConstPtr commandBuffer)

    wgpuSurfacePresent surface

    wgpuCommandBufferRelease commandBuffer
    wgpuCommandEncoderRelease commandEncoder
    wgpuTextureViewRelease frame
    wgpuTextureRelease surfaceTexture.texture

  print driverName

handleEvents ::
  Ptr WGPURenderPipeline ->
  Ptr WGPUPipelineLayout ->
  Ptr WGPUShaderModule ->
  Ptr WGPUSurfaceCapabilities ->
  Ptr WGPUQueue ->
  Ptr WGPUDevice ->
  Ptr WGPUAdapter ->
  Ptr WGPUSurface ->
  Ptr WGPUInstance ->
  IO ()
handleEvents a b c d e f g h i = alloca @SDL3.SDL_Event $ \event -> do
  pending <- SDL3.pollEvent event
  when pending $ do
    eventType <- peek (castPtr event :: Ptr SDL3.SDL_EventType)
    when (eventType == SDL3.SDL_EVENT_QUIT) $ quit a b c d e f g h i
    handleEvents a b c d e f g h i

quit ::
  Ptr WGPURenderPipeline ->
  Ptr WGPUPipelineLayout ->
  Ptr WGPUShaderModule ->
  Ptr WGPUSurfaceCapabilities ->
  Ptr WGPUQueue ->
  Ptr WGPUDevice ->
  Ptr WGPUAdapter ->
  Ptr WGPUSurface ->
  Ptr WGPUInstance ->
  IO ()
quit pipeline layout shader surfaceCap queue device adapter surface ins = do
  wgpuRenderPipelineRelease pipeline
  wgpuPipelineLayoutRelease layout
  wgpuShaderModuleRelease shader
  wgpuSurfaceCapabilitiesFreeMembers surfaceCap
  wgpuQueueRelease queue
  wgpuDeviceRelease device
  wgpuAdapterRelease adapter
  wgpuSurfaceRelease surface
  wgpuInstanceRelease ins
  exitSuccess

waitOnBool :: Ptr Bool -> IO ()
waitOnBool p = do
  waitOnBool p

-- b <- peek p
-- if b then pure () else waitOnBool p

onAdapterRequestCall :: WGPURequestAdapterCallback
onAdapterRequestCall status adapter _ _ u1 u2 = do
  unless (status == wGPURequestAdapterStatus_Success) $ error "Can't obtain adapter."
  when (status == wGPURequestAdapterStatus_Success) $ do
    print "Adapter ready!"
    hFlush stdout
    when (adapter == nullPtr) $ error "UHM"
    poke (castPtr u1) adapter

-- poke (castPtr u2) True

onDeviceRequestCall :: WGPURequestDeviceCallback
onDeviceRequestCall status device _ _ u1 u2 = do
  when (status == wGPURequestDeviceStatus_Success) $ do
    print "Device ready!"
    poke (castPtr u1) device
    poke (castPtr u2) True
