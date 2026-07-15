module Mischief.ECS.World.Spawn where

import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.World

spawnIO :: (Bundle b) => World -> b -> IO Entity
