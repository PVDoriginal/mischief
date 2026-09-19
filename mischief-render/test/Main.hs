{-# LANGUAGE MultiWayIf #-}
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
import Mischief.Assets (AssetSource (..), Image (..), load)
import Mischief.ECS
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as S
import Mischief.ECS.Time qualified as Time
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
import Mischief.Render.Image (QueueUpload (QueueUpload))
import Mischief.Render.Plugin
import Mischief.Render.Shader.Buffers
import Mischief.Render.Shader.Types hiding (Vec2)
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
main = do
  app <- newApp
  addPlugin @MainPlugin app
  runApp app

data MainPlugin

instance Plugin MainPlugin where
  init = do
    systems setup
      & schedule Startup

    systems moveSprite
      & schedule Update

    systems animSprite
      & after moveSprite
      & schedule Update

    insertRes $ AssetSource "../assets/"

  deps = [dep @RenderPlugin, dep @InputPlugin, dep @TimePlugin]

setup :: System ()
setup = do
  window <- spawn (Name "Window", Window, WindowSize 700 500)
  void $ spawn (Camera, Rel OutputTo window, def @Transform)

  spriteImage <- load @Image "characters.png"

  let positions = [V2 0 0, V2 (-1) 0, V2 1 0, V2 0 1, V2 0 (-1)]
  let characters = [Pink, Beige, Green, Purple, Yellow]

  for_ (positions `zip` characters) $ \(V2 x y, c) -> do
    let spritePos = V3 (x * 150) (y * 150) (-1)
    void $ spawn (Sprite spriteImage, def {translation = spritePos}, SpriteSlice {start = V2 516 387, size = V2 128 128}, c)

moveSprite :: System ()
moveSprite = do
  sprites <- query [q|E, Character, Transform / With Sprite|]
  Just keys <- res @Keys

  for_ sprites $ \(entity, character, sprite) -> do
    delta <- Time.delta
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
        insertIfNeq (CurrentSlices (walkAnims character)) entity
      else
        insertIfNeq (CurrentSlices (idleAnims character)) entity

    if
      | dir.x < 0 -> insert SpriteFlip {x = True, y = False} entity
      | dir.x > 0 -> insert SpriteFlip {x = False, y = False} entity
      | otherwise -> pure ()

    insert (Transform.translate (V3 dir.x dir.y 0) sprite) entity

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
  sprites <- query [q|Entity, AnimTimer, CurrentSlices|]
  delta <- Time.delta

  for_ sprites $ \(entity, AnimTimer (frame, timer), CurrentSlices slices) -> do
    let (timer', justFinished) = Timer.tick delta timer
    let frame' = if justFinished then (frame + 1) `mod` length slices else frame

    insert (slices !! frame') entity
    insert (AnimTimer (frame', timer')) entity

data Character = Pink | Purple | Green | Beige | Yellow deriving (Component, Show)

walkAnims :: Character -> [SpriteSlice]
walkAnims Pink = [SpriteSlice {start = V2 516 387, size = V2 128 128}, SpriteSlice {start = V2 645 387, size = V2 128 128}]
walkAnims Beige = [SpriteSlice {start = V2 0 129, size = V2 128 128}, SpriteSlice {start = V2 129 129, size = V2 128 128}]
walkAnims Green = [SpriteSlice {start = V2 258 258, size = V2 128 128}, SpriteSlice {start = V2 387 258, size = V2 128 128}]
walkAnims Purple = [SpriteSlice {start = V2 774 516, size = V2 128 128}, SpriteSlice {start = V2 0 645, size = V2 128 128}]
walkAnims Yellow = [SpriteSlice {start = V2 129 774, size = V2 128 128}, SpriteSlice {start = V2 258 774, size = V2 128 128}]

idleAnims :: Character -> [SpriteSlice]
idleAnims Pink = [SpriteSlice {start = V2 258 387, size = V2 128 128}]
idleAnims Beige = [SpriteSlice {start = V2 645 0, size = V2 128 128}]
idleAnims Green = [SpriteSlice {start = V2 0 258, size = V2 128 128}]
idleAnims Purple = [SpriteSlice {start = V2 516 516, size = V2 128 128}]
idleAnims Yellow = [SpriteSlice {start = V2 774 645, size = V2 128 128}]
