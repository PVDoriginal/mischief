import Control.Monad (forever, when)
import Data.ByteString qualified as BS
import Foreign (Ptr, Storable (peek), nullPtr)
import Foreign.C
import Foreign.C.ConstPtr
import Mischief.ECS.Prelude
import Mischief.WGPU
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
  let demo = newDemo {wgpuInstance}

  window <- withCString "sdl3-raw" $ \title -> SDL3.createWindow (ConstPtr title) 640 360 0

  windowProps <- SDL3.getWindowProperties window

  x <- unConstPtr <$> SDL3.getCurrentVideoDriver
  driverName <- peekCString x

  case driverName of
    "x11" -> do
      display <- BS.useAsCString SDL3.sDL_PROP_WINDOW_X11_DISPLAY_POINTER $ \b -> SDL3.getPointerProperty windowProps (ConstPtr b) nullPtr

      when (display == nullPtr) $ error "Can't obtain window display"
    _ -> undefined

  print driverName

-- forever $ pure ()