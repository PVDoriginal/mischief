module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable hiding (and)
import GHC.Generics hiding (C, C1)
import MischiefECS
import MischiefECS.App.Scheduler
import MischiefInput
import MischiefInput.Keyboard
import MischiefMath
import MischiefMath.Quat qualified as Quat
import MischiefMath.Transform (Transform)
import MischiefMath.Transform qualified as Transform
import MischiefMath.Vec
import SDL3
import Prelude hiding (and)

data Player = Player deriving (Queryable)

instance Component Player where
  required = require @Transform

main :: IO ()
main = do
  app <- newApp [sdlPlugin, timePlugin, keyboardPlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup setup
  addSystems Update movePlayer

setup :: System ()
setup = do
  _ <- spawn Player
  return ()

speed :: Float
speed = 100

movePlayer :: System ()
movePlayer = do
  Just keys <- single @Keys
  Just time <- single @Time

  Just player <- single' @Transform $ with @Player

  when (pressed SDL_SCANCODE_A keys) $ do
    modify player $ Transform.translate $ vec3 (-1, 0, 0) ^* (speed * time.value.deltaSecs)

  when (pressed SDL_SCANCODE_D keys) $ do
    modify player $ Transform.translate $ vec3 (1, 0, 0) ^* (speed * time.value.deltaSecs)

  when (pressed SDL_SCANCODE_W keys) $ do
    modify player $ Transform.translate $ vec3 (0, 1, 0) ^* (speed * time.value.deltaSecs)

  when (pressed SDL_SCANCODE_S keys) $ do
    modify player $ Transform.translate $ vec3 (0, -1, 0) ^* (speed * time.value.deltaSecs)

  when (pressed SDL_SCANCODE_Q keys) $ do
    modify player $ Transform.rotateX $ -(1 * speed * time.value.deltaSecs * 0.01)

  when (pressed SDL_SCANCODE_E keys) $ do
    modify player $ Transform.rotateX $ 1 * speed * time.value.deltaSecs * 0.01

  liftIO $ print player.value.translation
  liftIO $ print $ Quat.toEuler player.value.rotation
  liftIO $ putStrLn "\n"