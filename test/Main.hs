module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Data (typeOf)
import Data.Foldable
import MischiefECS

newtype Name = Name String deriving (Show, Component)

main :: IO ()
main = do
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystem Startup setup
  addSystem Update update

setup :: System ()
setup = do
  spawn (Name "Foo")
  spawn (Name "Bar")
  return ()

update :: System ()
update = do
  res <- query @(Entity, Name)
  for_ res $ \(entity, name) ->
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name