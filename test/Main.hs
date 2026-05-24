module Main where

import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.List qualified as List
import Data.Set qualified as Set
import GHC.Generics (Generic)
import MischiefECS.Prelude

newtype Name = Name String deriving (Generic, Show, Eq, Component, Queryable, Default)

data C = C deriving (Queryable, Show)

instance Component C where
  required = require @Name

main :: IO ()
main = do
  app <- newApp [plugin]
  runApp app

-- x :: DefaultBundleData
-- x = DefaultBundleData Set.empty

plugin :: Plugin ()
plugin = do
  addSystem Startup setup
  addSystem Update update

setup :: System ()
setup = do
  spawn (Name "Foo")
  e1 <- spawn (Name "Lol")
  insert C e1

  return ()

update :: System ()
update = do
  res <- query @(Entity, Name)

  for_ res $ \(entity, name) -> do
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name
