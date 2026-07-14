module MischiefECS.Relationships.Order where

import Data.Foldable
import Data.Maybe
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.Utils
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Query
import MischiefECS.World.Query.Queryable
import MischiefECS.World.Remove
import MischiefECS.World.Utils

data Before = Before deriving (Component)

data Visited = Visited deriving (Component)

orderEntities :: [Entity] -> System [Entity]
orderEntities entities = do
  res <- orderEntitiesStep entities
  for_ entities $ remove @Visited
  return res

orderEntitiesStep :: [Entity] -> System [Entity]
orderEntitiesStep entities = do
  next <- expect "Attempted to Order Cyclic Graph!" =<< findM isAvailable entities
  insert Visited next
  (next :) <$> orderEntitiesStep entities

isAvailable :: Entity -> System Bool
isAvailable entity = do
  Just before <- get @(Rel Before) entity
  isNothing <$> findM ((not . unwrap <$>) . (get @(Has Visited))) (map target before)
