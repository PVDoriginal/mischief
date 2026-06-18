module MischiefECS.Components.Spawn where

import Data.Data
import MischiefECS.Components

getOrAddPairId :: Pair -> Components -> IO ComponentId
getOrAddComponentId :: TypeRep -> Components -> IO ComponentId