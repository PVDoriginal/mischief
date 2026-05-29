module MischiefECS.SDL where

import Control.Monad
import Control.Monad.IO.Class
import MischiefECS.App
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

sdlPlugin :: Plugin ()
sdlPlugin = do
  liftIO initSdl
