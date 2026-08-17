{-# LANGUAGE MultiWayIf #-}

module Mischief.SDL
  ( -- $intro
    SDLPlugin (..),
    SDLMessage (..),
    Window (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Primitive.Ptr
import Foreign (Storable (peek), alloca, castPtr)
import Foreign.C
import Foreign.C.ConstPtr
import GHC.Records
import Mischief.ECS.Foreign as F
import Mischief.ECS.Messages (Message)
import Mischief.ECS.Messages qualified as Messages
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import SDL3.Sys qualified as SDL3
import System.Exit

newtype Window = Window {sdlWindow :: Ptr SDL3.SDL_Window}
  deriving anyclass (Component)

-- deriving newtype (Show)

initSdl :: IO (Ptr SDL3.SDL_Window)
initSdl = do
  version <- SDL3.getVersion
  let (major, rest) = fromIntegral version `divMod` (1000000 :: Int)
      (minor, micro) = rest `divMod` 1000
  putStrLn
    ( "sdl3-raw: linked against SDL "
        <> show major
        <> "."
        <> show minor
        <> "."
        <> show micro
    )

  -- The curated layer types the macro constant at the SDL_InitFlags
  -- newtype (the raw sDL_INIT_VIDEO CUInt stays available under
  -- SDL3.Sys.Bindgen.Init).
  ok <- SDL3.init SDL3.SDL_INIT_VIDEO
  unless ok (die "SDL_Init")

  window <- withCString "sdl3-raw" $ \title -> SDL3.createWindow (ConstPtr title) 640 360 0

  when (window == nullPtr) (die "Couldn't create window")

  pure window

data SDLMessage e = SDLMessage {eventType :: SDL3.SDL_EventType, event :: e}
  deriving anyclass (Message)
  deriving stock (Show)

data SDLPlugin = SDLPlugin deriving (Eq)

instance Plugin SDLPlugin where
  init _ = do
    window <- liftIO initSdl
    void $ spawn (Window window)
    Systems.add First handleEvents
    Systems.add First $ handleQuit `after` handleEvents

-- addMessage @(SDLMessage SDL3.SDL_DisplayEvent)
-- addMessage @(SDLMessage SDl3.SDL_WindowEvent)
-- addMessage @(SDLMessage SDL3.SDL_KeyboardEvent)
-- addMessage @(SDLMessage SDLMouseButtonEvent)
-- addMessage @(SDLMessage SDLMouseMotionEvent)
-- addMessage @(SDLMessage SDLQuitEvent)

-- addSystems First handleEvents
-- addSystems First $ handleQuit `after` handleEvents

handleEvents :: System ()
handleEvents = F.alloca @SDL3.SDL_Event $ \event -> do
  pending <- liftIO $ SDL3.pollEvent event
  when pending $ do
    e <- liftIO $ peek event
    eventType <- liftIO $ peek (castPtr event :: Ptr SDL3.SDL_EventType)
    handleEvent eventType e
    handleEvents

-- alloca @SDL3.SDL_Event $ \event -> do
--   liftIO SDL3.pollEventSafe
--   case event of
--     Nothing -> do
--       return ()
--     Just event -> do
--       handleEvent event
--       handleEvents

-- these correspond to https://wiki.libsdl.org/SDL3/SDL_Event
handleEvent :: SDL3.SDL_EventType -> SDL3.SDL_Event -> System ()
handleEvent t e = case t of
  -- audio
  SDL3.SDL_EVENT_AUDIO_DEVICE_ADDED -> s t e.adevice
  SDL3.SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED -> s t e.adevice
  SDL3.SDL_EVENT_AUDIO_DEVICE_REMOVED -> s t e.adevice
  -- TODO: camera
  -- TODO: clipboard
  -- display
  SDL3.SDL_EVENT_DISPLAY_ADDED -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_FIRST -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_LAST -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_MOVED -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_ORIENTATION -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_REMOVED -> s t e.display
  SDL3.SDL_EVENT_DISPLAY_USABLE_BOUNDS_CHANGED -> s t e.display
  -- TODO: gamepad
  -- TODO: gamepad axis
  -- TODO: gamepad button
  -- TODO: gamepad touchpad
  -- TODO: gamepad sensor
  -- TODO: drop
  -- TODO: finger
  -- TODO: pinch
  -- keyboard
  SDL3.SDL_EVENT_KEYBOARD_ADDED -> s t e.kdevice
  SDL3.SDL_EVENT_KEYBOARD_REMOVED -> s t e.kdevice
  -- key
  SDL3.SDL_EVENT_KEY_DOWN -> s t e.key
  SDL3.SDL_EVENT_KEY_UP -> s t e.key
  -- TODO: joystick
  -- TODO: joystick axis
  -- TODO: joystick ball
  -- TODO: joystick hat
  -- TODO: joystick battery
  -- TODO: joystick button
  -- mouse
  SDL3.SDL_EVENT_MOUSE_ADDED -> s t e.mdevice
  SDL3.SDL_EVENT_MOUSE_REMOVED -> s t e.mdevice
  -- mouse motion
  SDL3.SDL_EVENT_MOUSE_MOTION -> s t e.motion
  -- mouse button
  SDL3.SDL_EVENT_MOUSE_BUTTON_DOWN -> s t e.button
  SDL3.SDL_EVENT_MOUSE_BUTTON_UP -> s t e.button
  -- mouse wheel
  SDL3.SDL_EVENT_MOUSE_WHEEL -> s t e.wheel
  -- TODO: pen proximity
  -- TODO: pen touch
  -- TODO: pen motion
  -- TODO: pen button
  -- TODO: pen axis
  -- quit
  SDL3.SDL_EVENT_QUIT -> s t e.quit
  -- TODO: sensor
  -- TODO: edit
  -- TODO: edit candidates
  -- text
  SDL3.SDL_EVENT_TEXT_INPUT -> s t e.text
  -- user
  SDL3.SDL_EVENT_USER -> s t e.user
  -- window
  SDL3.SDL_EVENT_WINDOW_CLOSE_REQUESTED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_DESTROYED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_DISPLAY_CHANGED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_ENTER_FULLSCREEN -> s t e.window
  SDL3.SDL_EVENT_WINDOW_EXPOSED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_FIRST -> s t e.window
  SDL3.SDL_EVENT_WINDOW_FOCUS_GAINED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_FOCUS_LOST -> s t e.window
  SDL3.SDL_EVENT_WINDOW_HDR_STATE_CHANGED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_HIDDEN -> s t e.window
  SDL3.SDL_EVENT_WINDOW_HIT_TEST -> s t e.window
  SDL3.SDL_EVENT_WINDOW_ICCPROF_CHANGED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_LAST -> s t e.window
  SDL3.SDL_EVENT_WINDOW_LEAVE_FULLSCREEN -> s t e.window
  SDL3.SDL_EVENT_WINDOW_MAXIMIZED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_METAL_VIEW_RESIZED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_MINIMIZED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_MOUSE_ENTER -> s t e.window
  SDL3.SDL_EVENT_WINDOW_MOUSE_LEAVE -> s t e.window
  SDL3.SDL_EVENT_WINDOW_MOVED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_OCCLUDED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_RESIZED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_RESTORED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_SAFE_AREA_CHANGED -> s t e.window
  SDL3.SDL_EVENT_WINDOW_SHOWN -> s t e.window
  -- common
  t -> s t e.common
  where
    s t a = Messages.write $ SDLMessage t a

handleQuit :: System ()
handleQuit = do
  messages <- Messages.read @(SDLMessage SDL3.SDL_QuitEvent)
  unless (null messages) $ liftIO exitSuccess

-- $intro
-- This package provides the 'sdlPlugin' for @Mischief@, along with a few components.
--
-- These are necessary to run any SDL-based packages, such as @MischiefInput@, and @MischiefRender@.
--
-- In order to properly run this, you'll need to build / install SDL locally. This can be done via:
--
-- @
-- git clone --recurse-submodules https://github.com/PVDoriginal/mischief.git
-- cd mischief\/external\/SDL3
-- mkdir -p build && cd build
-- cmake -DCMAKE_BUILD_TYPE=Release -GNinja ..
-- cmake --build . --config Release --parallel
-- sudo cmake --install . --config Release   # omit sudo on Windows
-- @
--
-- On @Windows@, ensure @SDL3.dll@ is available in @PATH@ (or next to your .exe). If @pkg-config@ cannot find SDL3, set:
--
-- @
-- set PKG_CONFIG_PATH=path\to\SDL3\lib\pkgconfig
-- # or
-- \$env:PKG_CONFIG_PATH="path\to\SDL3\lib\pkgconfig"
-- @
--
-- Many thanks to [klukaszek](https://github.com/klukaszek) for the SDL3 bindings!