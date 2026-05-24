module Main where

import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.List qualified as List
import Data.Set qualified as Set
import GHC.Generics (Generic)
import MischiefECS.Prelude

newtype Counter = Counter Int deriving (Component, Queryable, Show)

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
  spawn (Counter 0)
  spawn (Name "Lol")

  return ()

update :: System ()
update = do
  res <- query @(Entity, (Counter, Name))

  for_ res $ \(entity, (counter, name)) -> do
    modify counter (\(Counter x) -> Counter $ x + 1)
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name ++ " and counter: " ++ show counter
