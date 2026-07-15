{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Components.Spawn where

import Data.Data
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.World

getOrAddPairId :: Pair -> System ComponentId
getOrAddComponentId :: ComponentType -> System ComponentId
meta :: forall c. (Component c) => System Entity