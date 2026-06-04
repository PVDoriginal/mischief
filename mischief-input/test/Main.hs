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
import SDL3 hiding (remove)
import Prelude hiding (and)

newtype Counter = Counter Int deriving (Component, Queryable, Show)

data E = E deriving (Event)

main :: IO ()
main = do
  app <- newApp [plugin, sdlPlugin, keyboardPlugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup setup
  addSystems Update system
  addObserver onInsertName

setup :: System ()
setup = do
  spawn (Counter 0, Name "Some name")
  return ()

system :: System ()
system = do
  Just keys <- single' @Keys $ with @Counter `and` changed @Keys
  when (justPressed SDL_SCANCODE_RETURN keys) $ do
    trigger E

onInsertName :: E -> System ()
onInsertName _ = do
  liftIO $ print "e was triggered"