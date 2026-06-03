module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import MischiefECS
import MischiefECS.App.Scheduler
import MischiefInput
import MischiefInput.Keyboard
import SDL3 hiding (remove)

newtype E = E String deriving (Event, Show)

data C = C deriving (Component)

main :: IO ()
main = do
  app <- newApp [sdlPlugin, keyboardPlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystem Startup $ do
    spawn (Name "lol")

    return ()

  addSystem Update update
  addObserver observer

update :: System ()
update = do
  Just keys <- single @Keys
  entities <- query @(Entity, Name)

  for_ entities $ \(entity, name) -> do
    when (justPressed SDL_SCANCODE_RETURN keys && name.value == Name "lol") $ do
      insert C entity

observer :: OnInsert Name -> System ()
observer (OnInsert entity) = do
  liftIO $ print $ "inserted component on entity " ++ show entity