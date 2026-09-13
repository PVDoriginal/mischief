{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}

{- HLINT ignore "Use newtype instead of data" -}

module Main where

import Control.Monad (replicateM_, void, when)
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.Function
import Data.List ((!?))
import Data.Traversable
import Mischief.ECS (ChildOf (..), addPlugin)
import Mischief.ECS.Components (From (..))
import Mischief.ECS.Interval qualified as Interval
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude
import Mischief.ECS.Relationships.ChildOf qualified as ChildOf
import Mischief.ECS.Relationships.Tree qualified as Tree
import Mischief.ECS.Stdin qualified as Stdin
import Mischief.ECS.Stdout
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.Timer (Timer)
import Mischief.ECS.Timer qualified as Timer
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Pipe (qinfo, qmap, qtraversal, qtraversal1)
import System.Exit
import System.Random
import System.Random.Stateful

main :: IO ()
main = do
  app <- newApp
  addPlugin @MainPlugin app
  runApp app

data Comp1 = Comp1 Int deriving (Component, Show)

data Position = Position Int deriving (Component, Show)

data Velocity = Velocity Int deriving (Component, Show)

data MainPlugin

instance Plugin MainPlugin where
  init = do
    -- a <- spawn (Name "A", Comp1 5, Position 2)
    -- b <- spawn (Name "B", Velocity 3, Rel ChildOf a)

    -- replicateM_ 10 $ qrun $ qmap (\(Velocity x, From a (Position y)) -> From a (Position $ x + y)) [q|Velocity, ChildOf -> (Position)|]
    -- warn . text =<< get a [q|Name, Comp1, Position|]

    a <- spawn (Name "A")
    b <- spawn (Name "B", Rel ChildOf a)
    c <- spawn (Name "C", Rel ChildOf a)
    d <- spawn (Name "D", Rel ChildOf b)
    e <- spawn (Name "E", Rel ChildOf b)
    f <- spawn (Name "F", Rel ChildOf c)

    g <- spawn (Name "G")
    h <- spawn (Name "H", Rel ChildOf g)
    i <- spawn (Name "I", Rel ChildOf g)

    [q|E|]
      & qtraversal1 (Tree.root @ChildOf) (const id) [q|Name|]
      & qinfo (("Root: " <>) . text)
      & get_ i

    [q|E|]
      & qtraversal (Tree.leaves @ChildOf) (const id) [q|Name|]
      & qinfo (("Leaves: " <>) . text)
      & get_ b

    [q|Name|]
      & qtraversal ChildOf.parent (,) [q|Name|]
      & qinfo (\(child, parent) -> text child <> " is child of " <> text parent)
      & query_

  deps = [dep @TimePlugin]
