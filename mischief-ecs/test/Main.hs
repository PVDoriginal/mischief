module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable hiding (and, or)
import Data.Maybe
import Data.Set qualified as Set
import GHC.Generics (Generic)
import MischiefECS
import MischiefECS.App.Systems
import MischiefECS.Components.Spawn
import Relationships (testRelationships)
import System.Exit (exitSuccess)

newtype A = A Int deriving (Queryable, Show, Eq)

instance Component A where
  required = require @B

data B = B deriving (Queryable, Show, Generic, Default)

instance Component B where
  required = require @C

data C = C deriving (Queryable, Generic, Default, Show)

instance Component C where
  required = require @()

main :: IO ()
main = do
  -- testRelationships
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  register @(A, B)
  register @ObserverOrder
  register @ComponentArchetypes

  initRes @C

  addSystems Startup setup
  addSystems Update dummy

  addSystems Update s1
  addSystems Update $ s2 `after` s1

setup :: System ()
setup = do
  _ <- spawn (A 5, Name "Lol")
  _ <- spawn (A 6, Name "Lmao")
  _ <- spawn (A 7, Name "wow")
  _ <- spawn (A 4, Name "idk")

  q <- query' @Name $ eq (A 5)
  traverse_ (liftIO . print) q

s1 :: System ()
s1 = do
  Just a5 <- single' @A $ check (== A 5)
  Just a6 <- single' @A $ check (== A 6)

  set a5 $ A 6
  set a6 $ A 5

s2 :: System ()
s2 = do
  q <- query' @(Name, A) $ changed @A
  for_ q $ \(name, a) -> do
    liftIO $ putStrLn $ show name ++ " " ++ show a

dummy :: System ()
dummy = return ()

newtype Counter = Counter Int
  deriving anyclass (Component, Queryable)
  deriving stock (Show)
