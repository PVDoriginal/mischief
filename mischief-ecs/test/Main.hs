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
import Mischief.ECS.Components.Common (Name)
import Mischief.ECS.Components.Hooks
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Hooks
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.TH
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

x :: System ()
x = do
  -- x <- $(quoteC ''ChildOf)
  -- let x = $quoteE
  undefined

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    a <- spawn ()
    x <- [q|Name, Name, ChildOf a, Name|]

    a <- spawn (Name "A")
    b <- spawn (Name "B")
    c <- spawn (Name "C")

    insert (Rel (TestRel 5) b, Rel (TestRel 10) c) a
    insert (Rel (TestRel 3) a, Rel (TestRel 4) c) b
    insert (Rel (TestRel 2) a, Rel (TestRel 6) b) c
    Systems.add Update (up, up2)

up :: System ()
up = do
  Just a <- single' E $ Check (== Name "A")
  Just b <- single' E $ Check (== Name "B")

  (info . text) =<< query' (C @Name) (Changed (R @TestRel a) |. Changed (R @TestRel b))

up2 :: System ()
up2 = do
  Just a <- single' E $ Check (== Name "A")
  Just b <- single' E $ Check (== Name "B")
  Just c <- single' E $ Check (== Name "C")

  insert (Rel (TestRel 5) a) b

-- x :: IO ()
-- x = print $ runQ [e|Just 5|]
