module MischiefECS.Relationships.ChildOf where

import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Relationships.Graph
import MischiefECS.World
import MischiefECS.World.Query
import MischiefECS.World.Query.Queryable

data ChildOf = ChildOf deriving (Show)

instance Component ChildOf where
  isExclusiveRel = True

parent :: forall m w. (MonadSystem w m) => Entity -> m (Maybe Entity)
parent entity = do
  p <- outgoing @ChildOf entity
  return $ case p of
    [p] -> Just p
    _ -> Nothing

children :: forall m w. (MonadSystem w m) => Entity -> m [Entity]
children = outgoing @ChildOf
