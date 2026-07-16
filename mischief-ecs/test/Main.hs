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
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.World.Utils
import System.Exit (exitSuccess)

newtype A = A Int deriving (Show, Eq)

instance Component A where
  required = require @B

data B = B deriving (Show, Generic, Default)

instance Component B where
  required = require @C

data C = C deriving (Generic, Default, Show)

instance Component C where
  required = require @()

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    Systems.add Startup setup
    Systems.add Update dummy

    void . spawn $ Observer onInsert
    void . spawn $ Observer onRemove

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

  e <- spawn ()
  despawn e
  insert (Name "") e

  q <- query' @Name $ with @A &. neg (without @B)
  traverse_ (info . text) q

dummy :: System ()
dummy = return ()

newtype Counter = Counter Int
  deriving anyclass (Component)
  deriving stock (Show)

onInsert :: OnInsert B -> System ()
onInsert _ = err "AAA"

onRemove :: OnRemove B -> System ()
onRemove _ = err "BBBB"