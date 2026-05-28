module Main where

import Control.Monad
import Control.Monad.IO.Class
import MischiefECS
import MischiefInput

main :: IO ()
main = do
  app <- newApp [inputPlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystem Update update

update :: System ()
update = do
  Just keys <- single @KeyManager
  when (isPressed Enter keys.value) $
    liftIO $
      print "aah"