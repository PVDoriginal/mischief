module MischiefSDL
  ( -- $intro
    sdlPlugin,
    SDLMessage (..),
    Window (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Mischief.ECS.App
import Mischief.ECS.App.Schedules
import Mischief.ECS.App.SystemConfig
import Mischief.ECS.Components
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Messages
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Spawn
import SDL3
import System.Exit

newtype Window = Window {sdlWindow :: SDLWindow}
  deriving anyclass (Component, Queryable)
  deriving newtype (Show)

initSdl :: IO SDLWindow
initSdl = do
  initSuccess <- liftIO $ sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]

  liftIO $ unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  Just w <- sdlCreateWindow "SDL3 Haskell Event Loop" 800 600 [SDL_WINDOW_RESIZABLE]
  return w

newtype SDLMessage e = SDLMessage e
  deriving anyclass (Message)
  deriving newtype (Show)

sdlPlugin :: Plugin ()
sdlPlugin = do
  window <- liftIO initSdl

  run $ do
    void $ spawn (Window window)
    x <- meta @Window
    a <- get @ComponentArchetypes x
    liftIO $ print a

  addMessage @(SDLMessage SDLDisplayEvent)
  addMessage @(SDLMessage SDLWindowEvent)
  addMessage @(SDLMessage SDLKeyboardEvent)
  addMessage @(SDLMessage SDLMouseButtonEvent)
  addMessage @(SDLMessage SDLMouseMotionEvent)
  addMessage @(SDLMessage SDLQuitEvent)

  addSystems First handleEvents
  addSystems First $ handleQuit `after` handleEvents

handleEvents :: System ()
handleEvents = do
  event <- liftIO sdlPollEvent
  case event of
    Nothing -> do
      return ()
    Just event -> do
      handleEvent event
      handleEvents

handleEvent :: SDLEvent -> System ()
handleEvent (SDLEventDisplay event) = do
  Just m <- res @(Messages (SDLMessage SDLDisplayEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventWindow event) = do
  Just m <- res @(Messages (SDLMessage SDLWindowEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventKeyboard event) = do
  Just m <- res @(Messages (SDLMessage SDLKeyboardEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventMouseButton event) = do
  Just m <- res @(Messages (SDLMessage SDLMouseButtonEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventMouseMotion event) = do
  Just m <- res @(Messages (SDLMessage SDLMouseMotionEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventQuit event) = do
  Just m <- res @(Messages (SDLMessage SDLQuitEvent))
  writeMessage (SDLMessage event) m
handleEvent _ = pure ()

handleQuit :: System ()
handleQuit = do
  Just msg <- res @(Messages (SDLMessage SDLQuitEvent))
  messages <- readMessages msg
  for_ messages $ \(SDLMessage _) -> do
    liftIO exitSuccess

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