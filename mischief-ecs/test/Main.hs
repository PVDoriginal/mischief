module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable hiding (and, or)
import Data.Maybe
import Data.Set qualified as Set
import GHC.Generics (Generic)
import GHC.StableName
import MischiefECS
import MischiefECS.App.Systems
import MischiefECS.Components.Common
import MischiefECS.Components.Spawn
import MischiefECS.World.Utils
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
  app <- newApp plugin
  runApp app

a :: Int -> Int
a x = x + 1

plugin :: Plugin ()
plugin = do
  register @(A, B)
  register @ObserverOrder
  register @ComponentArchetypes

  run $ addSystems Startup setup
  run $ addSystems Update dummy

setup :: System ()
setup = do
  _ <- spawn (A 5, Name "Lol")
  _ <- spawn (A 6, Name "Lmao")
  _ <- spawn (A 7, Name "wow")
  _ <- spawn (A 4, Name "idk")

  e <- spawn ()
  despawn e
  insert (Name "") e

  info "AAA"

  q <- query' @Name $ eq (A 5)
  traverse_ (liftIO . print) q

dummy :: System ()
dummy = return ()

newtype Counter = Counter Int
  deriving anyclass (Component, Queryable)
  deriving stock (Show)
