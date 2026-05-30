module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import MischiefECS
import MischiefInput
import MischiefInput.Keyboard
import SDL3 hiding (remove)

newtype E = E String deriving (Event, Show)

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
  entities <- query @Name

  for_ entities $ \name -> do
    when (justPressed SDL_SCANCODE_RETURN keys && name.value == Name "lol") $ do
      delete name

observer :: OnRemove Name -> System ()
observer (OnRemove entity) = do
  liftIO $ print $ "removed name on entity " ++ show entity