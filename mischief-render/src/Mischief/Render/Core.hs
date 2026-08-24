module Mischief.Render.Core where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Primitive (Ptr)
import Data.Primitive.Ptr (nullPtr)
import Foreign (castPtr, with)
import Foreign.C (peekCString)
import Foreign.C.ConstPtr
import Mischief.ECS.Events
import Mischief.ECS.Hooks
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.SDL (SDLPlugin (..), SDLWindow (SDLWindow))
import Mischief.WGPU
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General
import SDL3.Sys qualified as SDL3

newtype RenderInstance = RenderInstance (Ptr WGPUInstance) deriving anyclass (Component)

data RenderPlugin = RenderPlugin deriving (Eq)

instance Plugin RenderPlugin where
  init _ = do
    insertRes =<< liftIO (RenderInstance <$> wgpuCreateInstance)
    void $ Observers.spawn onAddWindow

  -- This plugin depends on SDLPlugin.
  plugins _ = plug SDLPlugin

newtype RenderSurface = RenderSurface (Ptr WGPUSurface)

instance Component RenderSurface where
  onRemove = hookSys $ \(HookContext entity) -> do
    Just (RenderSurface surface) <- [g|*RenderSurface|] entity
    liftIO $ wgpuSurfaceRelease surface

newtype RenderDevice = RenderDevice (Ptr WGPUDevice)

instance Component RenderDevice where
  onRemove = hookSys $ \(HookContext entity) -> do
    Just (RenderDevice device) <- [g|*RenderDevice|] entity
    liftIO $ wgpuDeviceRelease device

newtype RenderAdapter = RenderAdapter (Ptr WGPUAdapter)

instance Component RenderAdapter where
  onRemove = hookSys $ \(HookContext entity) -> do
    Just (RenderAdapter adapter) <- [g|*RenderAdapter|] entity
    liftIO $ wgpuAdapterRelease adapter

-- | Creates a WGPU Surface, Adapter, and Device for each new window. Inserts them as components.
onAddWindow :: OnAdd SDLWindow -> System ()
onAddWindow (OnAdd entity) = do
  Just (SDLWindow window) <- [g|*SDLWindow|] entity

  windowProps <- liftIO $ SDL3.getWindowProperties window
  driver <- liftIO (peekCString . unConstPtr =<< SDL3.getCurrentVideoDriver)

  Just (RenderInstance wgpuInstance) <- res @RenderInstance

  surfaceDescriptor <- liftIO $ getSurfaceDescriptor driver windowProps
  surface <- liftIO $ with surfaceDescriptor $ do
    wgpuInstanceCreateSurface wgpuInstance . ConstPtr

  when (surface == nullPtr) $ error "Couldn't create WGPU surface."

  adapter <- liftIO $ wgpuInstanceRequestAdapter wgpuInstance surface
  device <- liftIO $ wgpuAdapterRequestDevice adapter

  insert (RenderSurface surface, RenderAdapter adapter, RenderDevice device) entity

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