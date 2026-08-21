import Control.Concurrent
import Control.Monad (forever, unless, when)
import Data.ByteString qualified as BS
import Foreign (Ptr, Storable (peek, poke), alloca, castPtr, free, malloc, nullPtr, with)
import Foreign.C
import Foreign.C.ConstPtr
import Mischief.ECS.Prelude
import Mischief.WGPU
import Mischief.WGPU.Callbacks
import Mischief.WGPU.Enums
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types
import SDL3.Sys qualified as SDL3

data Demo = Demo
  { wgpuInstance :: Ptr WGPUInstance,
    surface :: Ptr WGPUSurface,
    adapter :: Ptr WGPUAdapter,
    device :: Ptr WGPUDevice,
    config :: Ptr WGPUSurfaceConfiguration
  }

newDemo :: Demo
newDemo = Demo nullPtr nullPtr nullPtr nullPtr nullPtr

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
    _ -> undefined

  adapter <- alloca @(Ptr WGPUAdapter) $ \adapterBox -> do
    with False $ \b -> do
      callback <- requestAdapterCallback onAdapterRequestCall
      let callbackInfo =
            newWGPURequestCallbackInfo
              { callback,
                userdata1 = castPtr adapterBox,
                userdata2 = castPtr b
              }

      let adapterOptions = newWGPURequestAdapterOptions {compatibleSurface = surface}

      with adapterOptions $ \adapterOptions -> do
        with callbackInfo $ \callbackInfo -> do
          wgpuInstanceRequestAdapter wgpuInstance adapterOptions callbackInfo
          waitOnBool b

          peek adapterBox

  when (adapter == nullPtr) $ error "Couldn't obtain WGPU adapter."

  device <- alloca @(Ptr WGPUDevice) $ \deviceBox -> do
    with False $ \b -> do
      callback <- requestDeviceCallback onDeviceRequestCall
      let callbackInfo =
            newWGPURequestCallbackInfo
              { callback,
                userdata1 = castPtr deviceBox,
                userdata2 = castPtr b
              }

      with callbackInfo $ \callbackInfo -> do
        wgpuAdapterRequestDevice adapter nullPtr callbackInfo
        waitOnBool b

        peek deviceBox

  when (device == nullPtr) $ error "Couldn't obtain WGPU edvice."

  queue <- wgpuDeviceGetQueue device

  when (queue == nullPtr) $ error "Couldn't obtain WGPU queue."

  print driverName

waitOnBool :: Ptr Bool -> IO ()
waitOnBool p = do
  b <- peek p
  if b then pure () else waitOnBool p

onAdapterRequestCall :: WGPURequestAdapterCallback
onAdapterRequestCall status adapter _ _ u1 u2 = do
  when (status == wGPURequestAdapterStatus_Success) $ do
    print "Adapter ready!"
    poke (castPtr u1) adapter
    poke (castPtr u2) True

onDeviceRequestCall :: WGPURequestDeviceCallback
onDeviceRequestCall status device _ _ u1 u2 = do
  when (status == wGPURequestDeviceStatus_Success) $ do
    print "Device ready!"
    poke (castPtr u1) device
    poke (castPtr u2) True