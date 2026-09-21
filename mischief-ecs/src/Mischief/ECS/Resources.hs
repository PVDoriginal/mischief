module Mischief.ECS.Resources (insertRes, res, resOrInsert) where

import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Spawn
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers

-- | Insert a resource into the World. If the resource already exists, its value will be overwritten.
insertRes :: forall r. (Component r, Bundle r) => r -> System ()
insertRes res = do
  entity <- meta @r
  insert res entity

-- | Get the value of a resource from the World.
res :: forall c. (Component c) => System (Maybe c)
res = do
  meta <- meta @c
  single $ mkGet meta (C @c)

-- | Get the value of a resource, or insert a value if it doesn't exist.
resOrInsert :: forall r. (Component r, Bundle r) => r -> System r
resOrInsert r = do
  meta <- meta @r
  x <- single $ mkGet meta (C @r)
  case x of
    Nothing -> do
      insertRes r
      pure r
    Just r -> pure r