module MischiefECS.Relationships.ChildOf where

import MischiefECS.Components
import MischiefECS.World.Query

data ChildOf = ChildOf deriving (Queryable, Show)

instance Component ChildOf where
  isExclusiveRel = True
