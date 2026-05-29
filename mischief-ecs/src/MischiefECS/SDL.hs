module MischiefECS.SDL where

import Control.Monad
import Control.Monad.IO.Class
import MischiefECS.App
import MischiefECS.Messages
import MischiefECS.World
import MischiefECS.World.Query
import SDL3
import System.Exit

initSdl :: IO ()
initSdl = do
  initSuccess <- liftIO $ sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]

  liftIO $ unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  _ <- sdlCreateWindow "SDL3 Haskell Event Loop" 800 600 [SDL_WINDOW_RESIZABLE]
  undefined

newtype SDLMessage e = SDLMessage e deriving (Message, Show)

data X = X

sdlPlugin :: Plugin ()
sdlPlugin = do
  liftIO initSdl

  addMessage @(SDLMessage SDLDisplayEvent)
  addMessage @(SDLMessage SDLWindowEvent)
  addMessage @(SDLMessage SDLKeyboardEvent)
  addMessage @(SDLMessage SDLMouseButtonEvent)
  addMessage @(SDLMessage SDLMouseMotionEvent)

  addSystem PreUpdate handleEvents

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
  Just m <- single @(Messages (SDLMessage SDLDisplayEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventWindow event) = do
  Just m <- single @(Messages (SDLMessage SDLWindowEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventKeyboard event) = do
  Just m <- single @(Messages (SDLMessage SDLKeyboardEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventMouseButton event) = do
  Just m <- single @(Messages (SDLMessage SDLMouseButtonEvent))
  writeMessage (SDLMessage event) m
handleEvent (SDLEventMouseMotion event) = do
  Just m <- single @(Messages (SDLMessage SDLMouseMotionEvent))
  writeMessage (SDLMessage event) m
handleEvent _ = pure ()