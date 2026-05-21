module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Data (typeOf)
import Data.Foldable
import Data.List qualified as List
import MischiefECS

newtype Name = Name String deriving (Show, Component, Eq, Queryable)

data C = C deriving (Component)

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
  e1 <- spawn (Name "Foo")
  spawn (Name "Bar")
  return ()

update :: System ()
update = do
  res <- query @(Entity, Name)

  for_ res $ \(entity, name) -> do
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name

    when (name.value == Name "Foo") $
      defer $
        set name $
          Name "Lol"

  flush