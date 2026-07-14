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
-- import Relationships (testRelationships)

import MischiefECS.App.Plugins
import MischiefECS.App.Systems
import MischiefECS.Components.Common
import MischiefECS.Components.Spawn
import MischiefECS.World.Utils
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
    addSystems Startup setup
    addSystems Update dummy

    void . spawn $ Observer onInsert

  plugins _ = plug Foo >. Bar >. Baz

data Foo = Foo deriving (Plugin, Eq)

data Bar = Bar deriving (Plugin, Eq)

data Baz = Baz deriving (Plugin, Eq)

setup :: System ()
setup = do
  c <- spawn (A 5, Name "Lol")
  _ <- spawn (A 6, Name "Lmao")
  _ <- spawn (A 7, Name "wow")
  _ <- spawn (A 4, Name "idk")

  remove @B c

  e <- spawn ()
  despawn e
  insert (Name "") e

  q <- query' @Name $ eq (A 5)
  traverse_ (info . text) q

dummy :: System ()
dummy = return ()

newtype Counter = Counter Int
  deriving anyclass (Component)
  deriving stock (Show)

onInsert :: OnRemove B -> System ()
onInsert _ = info "AAA"
