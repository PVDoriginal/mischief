module Main where

import Control.Monad
import Control.Monad.IO.Class
import MischiefECS
import MischiefInput
import MischiefInput.Keyboard
import SDL3

newtype E = E String deriving (Event, Show)

main :: IO ()
main = do
  app <- newApp [sdlPlugin, keyboardPlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystem Update update
  addObserver observer

update :: System ()
update = do
  Just keys <- single @Keys
  when (justPressed SDL_SCANCODE_RETURN keys) $ do
    trigger (E "lol")

observer :: E -> System ()
observer e = do
  liftIO $ print e