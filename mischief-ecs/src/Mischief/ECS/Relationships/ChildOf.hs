module Mischief.ECS.Relationships.ChildOf where

import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Relationships.Graph
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable

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
