import Codec.Picture qualified as P
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
import GHC.Generics
import Mischief.Assets (Image (..))
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as S
import Mischief.Render.Camera
import Mischief.Render.Core
import Mischief.Render.Plugin
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.Types
import Mischief.Render.Texture
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

  Just (CameraTexture texture) <- [g|*CameraTexture|] camera
  Just queue <- [g|*RenderQueue|] window
  Right image <- liftIO $ P.readImage "test/rat.jpg"
  uploadImage queue (Image image) texture

-- uploadTexture :: Ptr WGPUQueue -> Ptr WGPUTexture -> IO ()
-- uploadTexture queue texture = do
--   image' <- loadRGBA8 "test/rat.jpg"
--   let image = scaleBilinear 700 500 image'

--   let pixels = imageBytes image
--   let extent = WGPUExtent3D (fromIntegral image.imageWidth) (fromIntegral image.imageHeight) 1
--   let copyInfo =
--         WGPUTexelCopyTextureInfo
--           { texture,
--             mipLevel = 0,
--             origin = WGPUOrigin3D 0 0 0,
--             aspect = wGPUTextureAspect_All
--           }

--   let layout =
--         WGPUTexelCopyBufferLayout
--           { offset = 0,
--             bytesPerRow = 4 * fromIntegral image.imageWidth,
--             rowsPerImage = fromIntegral image.imageHeight
--           }

--   VS.unsafeWith (imageBytes image) $ \pixelPtr -> do
--     with extent $ \extent -> do
--       with copyInfo $ \copyInfo -> do
--         with layout $ \layout -> do
--           wgpuQueueWriteTexture queue (ConstPtr copyInfo) (ConstPtr $ castPtr pixelPtr) (fromIntegral $ VS.length pixels) (ConstPtr layout) (ConstPtr extent)

-- loadRGBA8 :: FilePath -> IO (Image PixelRGBA8)
-- loadRGBA8 path = do
--   -- bytes <- BS.readFile path
--   -- let Right result = decodeImage bytes
--   Right result <- readImage path
--   pure $ convertRGBA8 result

-- imageBytes :: Image PixelRGBA8 -> VS.Vector Word8
-- imageBytes (Image _ _ pixels) = pixels
