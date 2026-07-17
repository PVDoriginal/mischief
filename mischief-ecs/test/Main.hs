module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
-- import Relationships (testRelationships)

import Data.Data
import Data.Default
import Data.Foldable hiding (and, or)
import Data.Maybe
import Data.Set qualified as Set
import GHC.Generics (Generic)
import GHC.StableName
import Mischief.ECS
import Mischief.ECS.App.Plugins
import Mischief.ECS.App.Systems
import Mischief.ECS.Collectable
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.Hooks
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Hooks
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Utils
import System.Exit (exitSuccess)

newtype A = A Int deriving (Show, Eq)

instance Component A where
  required = require @B

data B = B deriving (Show, Generic, Default)

instance Component B where
  required = require @E

data E = E deriving (Generic, Default, Show)

instance Component E where
  required = require @()

data TestRel = TestRel

instance Component TestRel where
  hooks :: Hooks TestRel
  hooks = Hooks.relCleanupDespawn

data OtherRel = OtherRel deriving (Component, Show)

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    Systems.add Startup setup
    Systems.add Update dummy

  plugins _ = collect (Foo, Bar, Baz)

data Foo = Foo deriving (Plugin, Eq)

data Bar = Bar deriving (Plugin, Eq)

data Baz = Baz deriving (Plugin, Eq)

setup :: System ()
setup = do
  _ <- spawn (A 5, Name "Lol")
  c <- spawn (A 6, Name "Lmao")
  _ <- spawn (A 7, Name "wow")
  _ <- spawn (A 4, Name "idk")

  remove @B c

  e <- spawn (A 5)
  insert (Name "") e

  q <- query' (C @Name) (With @A, Without @B)

  traverse_ (info . text) q

  info $ text e

  x <- spawn (Rel (TestRel, e))
  despawn e
  (err . text) =<< isAlive x

dummy :: System ()
dummy = return ()

newtype Counter = Counter Int
  deriving anyclass (Component)
  deriving stock (Show)

onInsert :: OnInsert A -> System ()
onInsert _ = err "AAA"

onRemove :: OnRemove A -> System ()
onRemove _ = err "BBBB"
