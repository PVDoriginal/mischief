import Codec.Picture qualified as P
import Codec.Picture.Extra (scaleBilinear)
import Control.Concurrent
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Data (Proxy (..))
import Data.Default
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word
import Foreign (Bits ((.|.)), Ptr, Storable (alignment, peek, poke, sizeOf), alloca, allocaBytes, castPtr, free, malloc, mallocBytes, nullPtr, with)
import Foreign.C
import Foreign.C.ConstPtr
import GHC.Generics
import Mischief.Assets (Image (..), load)
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as S
import Mischief.Input (InputPlugin (InputPlugin))
import Mischief.Input.Keys (Keys)
import Mischief.Input.Keys qualified as Keys
import Mischief.Math
import Mischief.Math.Transform (Transform (..))
import Mischief.Math.Transform qualified as Transform
import Mischief.Render.Camera
import Mischief.Render.Core
import Mischief.Render.Plugin
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.Types
import Mischief.Render.Sprite
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
    S.add Update moveSprite

  plugins _ = plug (RenderPlugin, SpritePlugin, InputPlugin)

setup :: System ()
setup = do
  window <- spawn (Name "Window", Window, WindowSize 700 500)
  camera <- spawn (Camera, Rel OutputTo window, def @Transform)

  Just (CameraTexture texture) <- [g|*CameraTexture|] camera
  Just queue <- [g|*RenderQueue|] window
  Right image <- liftIO $ P.readImage "test/rat.jpg"
  uploadImage queue (Image image) texture

  spriteImage <- load @Image "test/rat.jpg"
  void $ spawn (Sprite spriteImage, def @Transform)

moveSprite :: System ()
moveSprite = do
  Just sprite <- [s|Transform / With Sprite|]
  Just keys <- res @Keys

  when (Keys.pressed Keys.A keys) $ do
    modify sprite $ Transform.translate (V3 (-1) 0 0)

  when (Keys.pressed Keys.D keys) $ do
    modify sprite $ Transform.translate (V3 1 0 0)

  when (Keys.pressed Keys.S keys) $ do
    modify sprite $ Transform.translate (V3 0 (-1) 0)

  when (Keys.pressed Keys.W keys) $ do
    modify sprite $ Transform.translate (V3 0 1 0)
