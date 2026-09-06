{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture qualified as P
import Codec.Picture.Extra (scaleBilinear)
import Control.Concurrent
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Data (Proxy (..))
import Data.Default
import Data.Foldable
import Data.IORef
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word
import Foreign (Bits ((.|.)), Ptr, Storable (alignment, peek, poke, sizeOf), alloca, allocaBytes, castPtr, free, malloc, mallocBytes, nullPtr, with)
import Foreign.C
import Foreign.C.ConstPtr
import GHC.Generics
import Mischief.Assets (Image (..), load)
import Mischief.ECS
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as S
import Mischief.ECS.Timer (Timer)
import Mischief.ECS.Timer qualified as Timer
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
    S.add Update $ animSprite `after` moveSprite

  plugins _ = plug (RenderPlugin, SpritePlugin, InputPlugin, TimePlugin)

setup :: System ()
setup = do
  window <- spawn (Name "Window", Window, WindowSize 700 500)
  camera <- spawn (Camera, Rel OutputTo window, def @Transform)

  Just (CameraTexture texture) <- [g|*CameraTexture|] camera
  Just queue <- res @RenderQueue
  Right image <- liftIO $ P.readImage "test/rat.jpg"
  liftIO $ uploadImage queue (Image image) texture

  spriteImage <- load @Image "test/characters.png"
  let spritePos = V3 0 0 (-1)
  void $ spawn (Sprite spriteImage, def {translation = spritePos}, SpriteSlice {start = V2 516 387, size = V2 128 128})

moveSprite :: System ()
moveSprite = do
  Just (entity, sprite) <- [s|E, Transform / With Sprite|]
  Just keys <- res @Keys

  delta <- deltaTime
  let speed = 150

  dir <- liftIO $ newIORef (V2 0 0)

  when (Keys.pressed Keys.A keys) $ do
    liftIO $ modifyIORef' dir (+ V2 (-1) 0)

  when (Keys.pressed Keys.D keys) $ do
    liftIO $ modifyIORef' dir (+ V2 1 0)

  when (Keys.pressed Keys.S keys) $ do
    liftIO $ modifyIORef' dir (+ V2 0 (-1))

  when (Keys.pressed Keys.W keys) $ do
    liftIO $ modifyIORef' dir (+ V2 0 1)

  dir <- liftIO $ (^* (delta * speed)) . normalize <$> readIORef dir

  if norm dir > 0
    then
      insertIfNeq (CurrentSlices walkAnims) entity
    else
      insertIfNeq (CurrentSlices idleAnims) entity

  if dir.x < 0
    then
      insert SpriteFlipX entity
    else
      remove (C @SpriteFlipX) entity

  modify sprite $ Transform.translate (V3 dir.x dir.y 0)

newtype CurrentSlices = CurrentSlices [SpriteSlice]
  deriving stock (Eq)

instance Component CurrentSlices where
  onSet =
    [ hook $ \(HookContext entity) -> do
        insert (AnimTimer (0, Timer.new 0.3 Timer.Repeat)) entity
    ]

newtype AnimTimer = AnimTimer (Int, Timer) deriving anyclass (Component)

animSprite :: System ()
animSprite = do
  Just (entity, AnimTimer (frame, timer), CurrentSlices slices) <- [s|Entity, *AnimTimer, *CurrentSlices|]
  delta <- deltaTime

  let (timer', justFinished) = Timer.tick delta timer
  let frame' = if justFinished then (frame + 1) `mod` length slices else frame

  insert (slices !! frame') entity
  insert (AnimTimer (frame', timer')) entity

walkAnims :: [SpriteSlice]
walkAnims = [SpriteSlice {start = V2 516 387, size = V2 128 128}, SpriteSlice {start = V2 645 387, size = V2 128 128}]

idleAnims :: [SpriteSlice]
idleAnims = [SpriteSlice {start = V2 258 387, size = V2 128 128}]