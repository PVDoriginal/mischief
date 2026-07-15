{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Relationships where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Mischief.ECS.Components
import Mischief.ECS.Relationships.ChildOf
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable

data IsExclusiveRelationship = IsExclusiveRelationship deriving (Show, Component)
