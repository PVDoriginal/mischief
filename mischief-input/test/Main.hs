module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import GHC.Generics hiding (C, C1)
import MischiefECS
import MischiefECS.App.Scheduler
import MischiefInput
import MischiefInput.Keyboard
import SDL3 hiding (remove)

newtype E = E String deriving (Event, Show)

data C = C

instance Component C where
  required = require @C1

data C1 = C1 deriving (Generic, Default)

instance Component C1 where
  required = require @C2

data C2 = C2 deriving (Generic, Default, Component, Queryable)

main :: IO ()
main = do
  app <- newApp [plugin, sdlPlugin, keyboardPlugin]
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
      liftIO $ print "inserted!"
      insert C entity

observer :: OnInsert Name -> System ()
observer (OnInsert entity) = do
  liftIO $ print $ "inserted component on entity " ++ show entity
