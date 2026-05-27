{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS
  ( module MischiefECS.Tables,
    module MischiefECS.World,
    module MischiefECS.Entities,
    module MischiefECS.Components.Bundle,
    module MischiefECS.Components.Default,
    module MischiefECS.Components,
    module MischiefECS.World.Query,
    module MischiefECS.World.Remove,
    module MischiefECS.App,
  )
where

import Data.Data
import MischiefECS.App
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Default
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query
import MischiefECS.World.Remove

class SystemParam s where
  type Result s

  eval :: Proxy s -> Result s

instance (Queryable qd) => SystemParam qd where
  type Result qd = QueryOutput qd

  eval :: forall k (qd :: k). (QueryData qd) => Proxy qd -> Result qd
  eval = undefined

system :: forall s. (SystemParam s) => (Result s -> System ()) -> System ()
system = undefined

test :: System ()
test =
  system @(Entity, Name) $ \(entity, name) -> do
    insert (Name "Lol") entity
