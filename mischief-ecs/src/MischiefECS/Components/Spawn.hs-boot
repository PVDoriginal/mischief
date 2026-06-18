module MischiefECS.Components.Spawn where

import Data.Data
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities.Internal
import MischiefECS.World

getOrAddPairId :: Pair -> Components -> System ComponentId
getOrAddComponentId :: TypeRep -> Components -> System ComponentId