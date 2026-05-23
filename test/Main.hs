module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Data (typeOf)
import Data.Default
import Data.Foldable
import Data.List qualified as List
import GHC.Generics (Generic)
import MischiefECS

newtype Name = Name String deriving (Generic, Show, Component, Eq, Queryable, Default)

data C = C deriving (Queryable)

instance Component C where
  required = require @(Name, Name)

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

  for_ res $ \(entity, name) -> do
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name
