module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Foldable hiding (and, or)
import Data.List qualified as List
import Data.Set qualified as Set
import GHC.Generics (Generic)
import MischiefECS
import MischiefECS.Prelude
import Prelude hiding (and, or)

newtype Counter = Counter Int deriving (Component, Queryable, Show)

data C1 = C1 String deriving (Component, Queryable, Show)

data C2 = C2 String deriving (Component, Queryable, Show)

data C3 = C3 String deriving (Component, Queryable, Show)

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
  spawn (Name "Lol", Counter $ -100000)

  return ()

update :: System ()
update = do
  res <- queryF @(Entity, (Counter, Name)) $ changed @(Name, Counter)

  for_ res $ \(entity, (counter, name)) -> do
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name ++ " and counter: " ++ show counter
