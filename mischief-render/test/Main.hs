import Codec.Picture
import Codec.Picture.Extra (scaleBilinear)
import Control.Concurrent
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Data (Proxy (..))
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word
import Foreign (Bits ((.|.)), Ptr, Storable (alignment, peek, poke, sizeOf), alloca, allocaBytes, castPtr, free, malloc, mallocBytes, nullPtr, with)
import Foreign.C
import Foreign.C.ConstPtr
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as S
import Mischief.Render.Camera
import Mischief.Render.Core
import Mischief.Render.Plugin
import Mischief.SDL.Window
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
main = runApp =<< newApp MainPlugin

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    S.add Startup setup

  plugins _ = plug RenderPlugin

setup :: System ()
setup = do
  window <- spawn (Name "Window", Window, WindowSize 700 500)
  camera <- spawn (Camera, Rel OutputTo window)

  Just (CameraTexture (Texture tex)) <- [g|*CameraTexture|] camera
  Just (RenderQueue queue) <- [g|*RenderQueue|] window
  liftIO $ uploadTexture queue tex

uploadTexture :: Ptr WGPUQueue -> Ptr WGPUTexture -> IO ()
uploadTexture queue texture = do
  image' <- loadRGBA8 "test/rat.jpg"
  let image = scaleBilinear 700 500 image'

  let pixels = imageBytes image
  let extent = WGPUExtent3D (fromIntegral image.imageWidth) (fromIntegral image.imageHeight) 1
  let copyInfo =
        WGPUTexelCopyTextureInfo
          { texture,
            mipLevel = 0,
            origin = WGPUOrigin3D 0 0 0,
            aspect = wGPUTextureAspect_All
          }

  let layout =
        WGPUTexelCopyBufferLayout
          { offset = 0,
            bytesPerRow = 4 * fromIntegral image.imageWidth,
            rowsPerImage = fromIntegral image.imageHeight
          }

  VS.unsafeWith (imageBytes image) $ \pixelPtr -> do
    with extent $ \extent -> do
      with copyInfo $ \copyInfo -> do
        with layout $ \layout -> do
          wgpuQueueWriteTexture queue (ConstPtr copyInfo) (ConstPtr $ castPtr pixelPtr) (fromIntegral $ VS.length pixels) (ConstPtr layout) (ConstPtr extent)

test :: IO ()
test = do
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
    _ -> undefined

  adapter <- wgpuInstanceRequestAdapter wgpuInstance surface
  device <- wgpuAdapterRequestDevice adapter

  -- image <- loadRGBA8 "test/rat.jpg"
  -- Right result <- readImage "test/rat.jpg"
  -- let img = convertRGBA8 result
  !image <- BS.readFile "test/rat.jpg"
  -- print $ image.imageWidth
  -- print $ image.imageHeight

  let Right !img' = decodeImage image
  let !img = convertRGBA8 img'

  let x :: VS.Vector Word8 = VS.generate (2916 * 1988) (const 54)

  VS.unsafeWith x $ \_ -> pure ()

  -- threadDelay 5000000

  texture <- withWGPUString "A" $ \label -> do
    let desc = textureDescriptor 2916 1988 label
    with desc $ \desc -> wgpuDeviceCreateTexture device (ConstPtr desc)

  allocaBytes 64 $ pure
  allocaBytes 64 $ pure
  allocaBytes 64 $ pure
  allocaBytes 64 $ pure
  allocaBytes 64 $ pure
  allocaBytes 64 $ pure
  -- allocaBytes 64 $ pure
  -- allocaBytes 64 $ pure
  allocaBytes 64 $ pure
  -- allocaBytes 64 $ pure
  -- allocaBytes 64 $ pure

  -- shaderModule <- loadShaderModule device "test/shader.wgsl"
  -- when (shaderModule == nullPtr) $ error "Couldn't load shader module."
  -- shaderModule <- loadShaderModule device "test/shader.wgsl"
  -- when (shaderModule == nullPtr) $ error "Couldn't load shader module."
  shaderModule <- loadShaderModule device "test/shader.wgsl"
  when (shaderModule == nullPtr) $ error "Couldn't load shader module."

  let pipelineLayoutDesc = newWGPUPipelineLayoutDescriptor
  pipelineLayout <- with pipelineLayoutDesc $ \pipelineLayoutDesc -> do
    --   print "A"
    wgpuDeviceCreatePipelineLayout device (ConstPtr pipelineLayoutDesc)

  -- print "C"

  pure ()

-- main :: IO ()
-- main = test

_main :: IO ()
_main = do
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

  (texture, w, h) <- loadTexture device queue

  textureView <- wgpuTextureCreateView texture (ConstPtr nullPtr)
  -- let viewDesc = WGPUTexture

  -- wgpuTextureRelease texture
  let samplerDesc =
        WGPUSamplerDescriptor
          { addressModeU = wGPUAddressMode_ClampToEdge,
            addressModeV = wGPUAddressMode_ClampToEdge,
            addressModeW = wGPUAddressMode_ClampToEdge,
            magFilter = wGPUFilterMode_Linear,
            minFilter = wGPUFilterMode_Linear,
            mipmapFilter = wGPUMipmapFilterMode_Nearest,
            nextInChain = nullPtr,
            label = WGPUStringView (ConstPtr nullPtr) 0,
            lodMinClamp = 0,
            lodMaxClamp = 32,
            compare = wGPUCompareFunction_Undefined,
            maxAnisotropy = 1
          }
  print $ alignment samplerDesc
  -- alloca @WGPUSamplerDescriptor $ \_ -> pure ()
  sampler <- with samplerDesc $ wgpuDeviceCreateSampler device . ConstPtr
  -- with samplerDesc $ \_ -> pure ()
  when (sampler == nullPtr) $ error "Couldn't create sampler"

  shaderModule <- loadShaderModule device "test/shader.wgsl"
  when (shaderModule == nullPtr) $ error "Couldn't load shader module."

  surfaceCapabilities <- malloc @WGPUSurfaceCapabilities
  wgpuSurfaceGetCapabilities surface adapter surfaceCapabilities

  cap <- peek surfaceCapabilities
  free surfaceCapabilities
  let (ConstPtr formats) = cap.formats
  let (ConstPtr alphaModes) = cap.alphaModes
  format <- peek formats
  alphaMode <- peek alphaModes

  let bindGroupLayoutEntries =
        VS.fromList
          [ WGPUBindGroupLayoutEntry
              { nextInChain = nullPtr,
                binding = 0,
                visibility = wGPUShaderStage_Fragment,
                bindingArraySize = 0,
                buffer = unusedBufferLayout,
                sampler = unusedSamplerLayout,
                storageTexture = unusedStorageTextureLayout,
                texture =
                  WGPUTextureBindingLayout
                    { sampleType = wGPUTextureSampleType_Float,
                      multisampled = wgpuFalse,
                      viewDimension = wGPUTextureViewDimension_2D,
                      nextInChain = nullPtr
                    }
              },
            WGPUBindGroupLayoutEntry
              { nextInChain = nullPtr,
                binding = 1,
                visibility = wGPUShaderStage_Fragment,
                bindingArraySize = 0,
                buffer = unusedBufferLayout,
                storageTexture = unusedStorageTextureLayout,
                texture = unusedTextureLayout,
                sampler =
                  WGPUSamplerBindingLayout
                    { _type = wGPUSamplerBindingType_Filtering,
                      nextInChain = nullPtr
                    }
              }
          ]

  bindGroupLayout <- VS.unsafeWith bindGroupLayoutEntries $ \entries -> do
    let desc =
          WGPUBindGroupLayoutDescriptor
            { entryCount = 2,
              entries = ConstPtr entries,
              label = WGPUStringView (ConstPtr nullPtr) 0,
              nextInChain = nullPtr
            }
    with desc $ \desc -> wgpuDeviceCreateBindGroupLayout device (ConstPtr desc)

  pipelineLayoutDesc <- with bindGroupLayout $ \bindGroupLayout -> do
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

  when (pipelineLayout == nullPtr) $ error "Couldn't generate pipeline layout."

  pipeline <- withWGPUString "vs_main" $ \vertexEntry -> do
    withWGPUString "fs_main" $ \fragmentEntry -> do
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

  bindGroup <- withWGPUString "bind group" $ \label -> do
    let entries =
          VS.fromList
            [ WGPUBindGroupEntry
                { binding = 0,
                  textureView = textureView,
                  size = 0,
                  offset = 0,
                  sampler = nullPtr,
                  buffer = nullPtr,
                  nextInChain = nullPtr
                },
              WGPUBindGroupEntry
                { binding = 1,
                  textureView = nullPtr,
                  size = 0,
                  offset = 0,
                  sampler,
                  buffer = nullPtr,
                  nextInChain = nullPtr
                }
            ]

    VS.unsafeWith entries $ \entries -> do
      let desc =
            WGPUBindGroupDescriptor
              { layout = bindGroupLayout,
                entryCount = 2,
                label = label,
                nextInChain = nullPtr,
                entries = ConstPtr entries
              }

      with desc $ \desc -> wgpuDeviceCreateBindGroup device (ConstPtr desc)

  _ <- forever $ do
    with cap $ \cap ->
      handleEvents pipeline pipelineLayout shaderModule cap queue device adapter surface wgpuInstance

    surfaceTextureBox <- malloc
    wgpuSurfaceGetCurrentTexture surface surfaceTextureBox
    surfaceTexture <- peek surfaceTextureBox
    free surfaceTextureBox

    frame <- wgpuTextureCreateView surfaceTexture.texture (ConstPtr nullPtr)
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
    wgpuRenderPassEncoderSetBindGroup renderPassEncoder 0 bindGroup 0 (ConstPtr nullPtr)
    wgpuRenderPassEncoderDraw renderPassEncoder 3 1 0 0
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
    wgpuSurfacePresent surface
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

loadTexture :: Ptr WGPUDevice -> Ptr WGPUQueue -> IO (Ptr WGPUTexture, Int, Int)
loadTexture device queue = do
  image <- loadRGBA8 "test/rat.jpg"

  texture <- withWGPUString "A" $ \label -> do
    let desc = textureDescriptor (fromIntegral image.imageWidth) (fromIntegral image.imageHeight) label
    with desc $ \desc -> wgpuDeviceCreateTexture device (ConstPtr desc)

  let pixels = imageBytes image
  let extent = WGPUExtent3D (fromIntegral image.imageWidth) (fromIntegral image.imageHeight) 1
  let copyInfo =
        WGPUTexelCopyTextureInfo
          { texture,
            mipLevel = 0,
            origin = WGPUOrigin3D 0 0 0,
            aspect = wGPUTextureAspect_All
          }

  let layout =
        WGPUTexelCopyBufferLayout
          { offset = 0,
            bytesPerRow = 4 * fromIntegral image.imageWidth,
            rowsPerImage = fromIntegral image.imageHeight
          }

  VS.unsafeWith (imageBytes image) $ \pixelPtr -> do
    with extent $ \extent -> do
      with copyInfo $ \copyInfo -> do
        with layout $ \layout -> do
          wgpuQueueWriteTexture queue (ConstPtr copyInfo) (ConstPtr $ castPtr pixelPtr) (fromIntegral $ VS.length pixels) (ConstPtr layout) (ConstPtr extent)

  pure (texture, image.imageWidth, image.imageHeight)

loadRGBA8 :: FilePath -> IO (Image PixelRGBA8)
loadRGBA8 path = do
  -- bytes <- BS.readFile path
  -- let Right result = decodeImage bytes
  Right result <- readImage path
  pure $ convertRGBA8 result

imageBytes :: Image PixelRGBA8 -> VS.Vector Word8
imageBytes (Image _ _ pixels) = pixels

textureDescriptor :: Word32 -> Word32 -> WGPUStringView -> WGPUTextureDescriptor
textureDescriptor w h label =
  WGPUTextureDescriptor
    { size = WGPUExtent3D w h 1,
      label,
      mipLevelCount = 1,
      sampleCount = 1,
      dimension = wGPUTextureDimension_2D,
      format = wGPUTextureFormat_RGBA8Unorm,
      usage = wGPUTextureUsage_TextureBinding .|. wGPUTextureUsage_CopyDst,
      nextInChain = nullPtr,
      viewFormatCount = 0,
      viewFormats = ConstPtr nullPtr
    }

textureViewDescriptor :: Word32 -> Word32 -> WGPUStringView -> WGPUTextureViewDescriptor
textureViewDescriptor w h label =
  WGPUTextureViewDescriptor
    { baseMipLevel = 0,
      arrayLayerCount = 1,
      mipLevelCount = 1,
      dimension = wGPUTextureViewDimension_2D,
      aspect = wGPUTextureAspect_All,
      usage = wGPUTextureUsage_TextureBinding .|. wGPUTextureUsage_CopyDst,
      format = wGPUTextureFormat_RGBA8Unorm,
      label = label,
      nextInChain = nullPtr
    }