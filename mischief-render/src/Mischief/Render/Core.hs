module Mischief.Render.Core where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Default (Default)
import Data.Maybe (isNothing)
import Data.Primitive (Ptr)
import Data.Primitive.Ptr (nullPtr)
import Foreign (Storable (peek), castPtr, free, malloc, with)
import Foreign.C (peekCString)
import Foreign.C.ConstPtr
import Mischief.ECS.Events
import Mischief.ECS.Hooks
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.SDL (SDLPlugin (..), SDLWindow (SDLWindow))
import Mischief.SDL.Window
import Mischief.WGPU
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General
import SDL3.Sys qualified as SDL3

newtype RenderInstance = RenderInstance (Ptr WGPUInstance) deriving anyclass (Component)

newtype RenderSurface = RenderSurface (Ptr WGPUSurface)

instance Component RenderSurface where
  onRemove =
    [ hook $ \(HookContext entity) -> do
        Just (RenderSurface surface) <- [g|*RenderSurface|] entity
        liftIO $ wgpuSurfaceRelease surface
    ]

newtype RenderDevice = RenderDevice (Ptr WGPUDevice)

instance Component RenderDevice where
  onRemove =
    [ hook $ \(HookContext entity) -> do
        Just (RenderDevice device) <- [g|*RenderDevice|] entity
        liftIO $ wgpuDeviceRelease device
    ]

newtype RenderAdapter = RenderAdapter (Ptr WGPUAdapter)

instance Component RenderAdapter where
  onRemove =
    [ hook $ \(HookContext entity) -> do
        Just (RenderAdapter adapter) <- [g|*RenderAdapter|] entity
        liftIO $ wgpuAdapterRelease adapter
    ]

newtype RenderQueue = RenderQueue (Ptr WGPUQueue)

instance Component RenderQueue where
  onRemove =
    [ hook $ \(HookContext entity) -> do
        Just (RenderQueue queue) <- [g|*RenderQueue|] entity
        liftIO $ wgpuQueueRelease queue
    ]

-- | Creates a WGPU Surface, Adapter, and Device for each new window. Inserts them as components.
onAddWindow :: OnAdd SDLWindow -> System ()
onAddWindow (OnAdd entity) = do
  Just (SDLWindow window, WindowSize w h) <- [g|*SDLWindow, *WindowSize|] entity

  windowProps <- liftIO $ SDL3.getWindowProperties window
  driver <- liftIO (peekCString . unConstPtr =<< SDL3.getCurrentVideoDriver)

  Just (RenderInstance wgpuInstance) <- res @RenderInstance

  surfaceDescriptor <- liftIO $ getSurfaceDescriptor driver windowProps
  surface <- liftIO $ with surfaceDescriptor $ do
    wgpuInstanceCreateSurface wgpuInstance . ConstPtr

  when (surface == nullPtr) $ error "Couldn't create WGPU surface."

  tryAdapter <- res @RenderAdapter
  (adapter, device, _) <- case tryAdapter of
    Just (RenderAdapter adapter) -> do
      Just (RenderDevice device) <- res @RenderDevice
      Just (RenderQueue queue) <- res @RenderQueue
      pure (adapter, device, queue)
    Nothing -> do
      adapter <- liftIO $ wgpuInstanceRequestAdapter wgpuInstance surface
      device <- liftIO $ wgpuAdapterRequestDevice adapter
      queue <- liftIO $ wgpuDeviceGetQueue device

      insertRes (RenderAdapter adapter)
      insertRes (RenderDevice device)
      insertRes (RenderQueue queue)
      pure (adapter, device, queue)

  surfaceCapabilities <- liftIO $ malloc @WGPUSurfaceCapabilities
  liftIO $ wgpuSurfaceGetCapabilities surface adapter surfaceCapabilities

  cap <- liftIO $ peek surfaceCapabilities
  liftIO $ free surfaceCapabilities
  let (ConstPtr formats) = cap.formats
  let (ConstPtr alphaModes) = cap.alphaModes
  format <- liftIO $ peek formats
  alphaMode <- liftIO $ peek alphaModes

  let config =
        newWGPUSurfaceConfiguration
          { device,
            usage = wGPUTextureUsage_RenderAttachment,
            format,
            presentMode = wGPUPresentMode_Fifo,
            alphaMode,
            width = fromIntegral w,
            height = fromIntegral h
          }

  liftIO $ with config $ \config -> do
    wgpuSurfaceConfigure surface (ConstPtr config)

  insert (RenderSurface surface) entity

-- | Maps a SDL driver name and window to the correct surface texture for WGPU.
--
-- TODO: implement the rest of the drivers.
getSurfaceDescriptor :: String -> SDL3.SDL_PropertiesID -> IO WGPUSurfaceDescriptor
getSurfaceDescriptor "x11" props = do
  display <- BS.useAsCString SDL3.sDL_PROP_WINDOW_X11_DISPLAY_POINTER $ \b -> SDL3.getPointerProperty props (ConstPtr b) nullPtr
  window <- BS.useAsCString SDL3.sDL_PROP_WINDOW_X11_WINDOW_NUMBER $ \b -> SDL3.getNumberProperty props (ConstPtr b) (-1)

  when (display == nullPtr || window == -1) $ error "Can't obtain x11 window."

  let chain = WGPUChainedStruct {next = nullPtr, sType = wGPUSType_SurfaceSourceXlibWindow}
  let xlib = WGPUSurfaceSourceXlibWindow {chain, display, window}

  with xlib $ \xlib ->
    pure
      WGPUSurfaceDescriptor
        { nextInChain = castPtr xlib,
          label = WGPUStringView {_data = ConstPtr nullPtr, length = 0}
        }
getSurfaceDescriptor "windows" props = do
  hwnd <- BS.useAsCString SDL3.sDL_PROP_WINDOW_WIN32_HWND_POINTER $ \b -> SDL3.getPointerProperty props (ConstPtr b) nullPtr
  hinstance <- BS.useAsCString SDL3.sDL_PROP_WINDOW_WIN32_INSTANCE_POINTER $ \b -> SDL3.getPointerProperty props (ConstPtr b) nullPtr

  let chain = WGPUChainedStruct {next = nullPtr, sType = wGPUSType_SurfaceSourceWindowsHWND}
  let win = WGPUSurfaceSourceWindowsHWND {chain, hinstance, hwnd}

  with win $ \win ->
    pure $
      WGPUSurfaceDescriptor
        { nextInChain = castPtr win,
          label = WGPUStringView {_data = ConstPtr nullPtr, length = 0}
        }
getSurfaceDescriptor s _ = error $ "Incompatible driver: " ++ s

newtype BindLayout = BindLayout (Ptr WGPUBindGroupLayout)

newtype Pipeline = Pipeline (Ptr WGPURenderPipeline)

newtype TextureFormat = TextureFormat WGPUTextureFormat

newtype Sampler = Sampler (Ptr WGPUSampler)

getRenderingResources :: System (Maybe (RenderAdapter, RenderDevice, RenderQueue))
getRenderingResources = do
  adapter <- res @RenderAdapter
  device <- res @RenderDevice
  queue <- res @RenderQueue
  pure $ (,,) <$> adapter <*> device <*> queue