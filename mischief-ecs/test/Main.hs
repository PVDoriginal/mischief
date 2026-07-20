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

data Player = Player deriving (Component, Show)

data CompA = CompA Int deriving (Component, Eq, Show)

data CompB = CompB String deriving (Component, Eq, Show)

data TestRel = TestRel Int deriving (Component, Show, Eq)

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    a <- spawn (Name "A")
    c <- spawn (Name "C", Rel (TestRel 5) a)
    b <- spawn (Name "B", Rel (TestRel 5) a, Rel (TestRel 6) c)

    (info . text) =<< query' (C @Name) (CheckR Any (== TestRel 5))
    (info . text) =<< query' (C @Name) (CheckR Any (== TestRel 7))
    (info . text) =<< query' (C @Name) (CheckR a (== TestRel 6))
    (info . text) =<< query' (C @Name) (CheckR c (== TestRel 6))

    a <- query (Val (C @Name, C @Name, R @Name Any))
    undefined