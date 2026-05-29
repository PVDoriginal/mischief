module Main where

import Control.Monad
import Control.Monad.IO.Class
import MischiefECS
import MischiefInput
import MischiefInput.Keyboard
import SDL3

main :: IO ()
main = do
  app <- newApp [sdlPlugin, keyboardPlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystem Update update

update :: System ()
update = do
  Just keys <- single @Keys
  when (justPressed SDL_SCANCODE_RETURN keys) $ do
    liftIO $ print "Lol"