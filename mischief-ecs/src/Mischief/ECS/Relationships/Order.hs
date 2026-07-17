module Mischief.ECS.Relationships.Order where

import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.Utils
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Utils

data Before = Before deriving (Component)

data Visited = Visited deriving (Component)

orderEntities :: [Entity] -> System [Entity]
orderEntities entities = do
  res <- orderEntitiesStep (Set.fromList entities)
  for_ entities $ remove (C @Visited)
  -- err $ text res
  return res

orderEntitiesStep :: Set Entity -> System [Entity]
orderEntitiesStep entities =
  if null entities
    then
      return []
    else do
      next <- expect "Attempted to Order Cyclic Graph!" =<< findM isAvailable entities
      insert Visited next
      (next :) <$> orderEntitiesStep (Set.delete next entities)

isAvailable :: Entity -> System Bool
isAvailable entity = do
  before <- query' (C @Entity) (WithR @Before entity)
  isNothing <$> findM ((not . unwrap <$>) . get (Has @Visited)) before