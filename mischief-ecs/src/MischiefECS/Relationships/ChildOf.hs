module MischiefECS.Relationships.ChildOf where

import MischiefECS.Components
import MischiefECS.World.Query

data ChildOf = ChildOf deriving (Component, Queryable, Show)
