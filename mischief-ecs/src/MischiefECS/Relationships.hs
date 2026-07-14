{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Relationships where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import MischiefECS.Components
import MischiefECS.Relationships.ChildOf
import MischiefECS.World.Query
import MischiefECS.World.Query.Queryable

data IsExclusiveRelationship = IsExclusiveRelationship deriving (Show, Component)
