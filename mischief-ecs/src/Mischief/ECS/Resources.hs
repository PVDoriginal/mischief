module Mischief.ECS.Resources where

import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryType
import Mischief.ECS.World.Query.Queryable

-- | Insert a resource into this world. If the resource already exists, its value will be overwritten.
insertRes :: forall r. (Component r, Bundle r) => r -> System ()
insertRes res = do
  entity <- meta @r
  insert res entity

res :: forall c. (QueryType c) => System (Maybe c)
res = do
  meta <- meta @c
  get (Val (C @c)) meta

resOrInsert :: forall r. (Component r, Updateable (Result r), Bundle r) => r -> System (Result r)
resOrInsert r = do
  meta <- meta @r
  getOrInsert r meta
