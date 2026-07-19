{- HLINT ignore "Use newtype instead of data" -}
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

data CompA = CompA Int Int deriving (Component, Show)

data CompB = CompB String deriving (Component, Show)

data CompC = CompC deriving (Component, Show)

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    foo <- spawn (Name "Foo", CompA 10 10, CompB "Component B on Foo", CompC)
    bar <- spawn (Name "Bar", CompA 15 3, CompB "Component B on Bar")
    baz <- spawn (Name "Baz", CompA 0 0, CompB "Component B on Baz", CompC)

    info . text =<< query (C @Name, C @CompA, M @CompB, M @CompC)

    insert (Name "Foo2", CompA 100 100, CompC) foo
    remove (C @CompC, C @CompB) baz
    insert (CompA 150 5) bar

    info . text =<< query (C @Name, C @CompA, M @CompB, M @CompC)
