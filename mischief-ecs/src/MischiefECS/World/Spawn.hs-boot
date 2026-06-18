module MischiefECS.World.Spawn where

import MischiefECS.Components.Bundle
import MischiefECS.Entities.Internal
import MischiefECS.World

spawnIO :: (Bundle b) => World -> b -> IO Entity