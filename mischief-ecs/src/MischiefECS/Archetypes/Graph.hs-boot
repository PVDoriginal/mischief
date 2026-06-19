module MischiefECS.Archetypes.Graph where

import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import MischiefECS.Components
import MischiefECS.Vec
import MischiefECS.World

data ArchetypeGraph = ArchetypeGraph {nodes :: IOVec ArchetypeNode, lookup :: IORef (Map (Set ComponentId) Int), counter :: IORef Int}

newArchetypeGraph :: IO ArchetypeGraph

data ArchetypeNode = ArchetypeNode
  { archetype :: ArchetypeData,
    insert :: Map ComponentId Int,
    remove :: Map ComponentId Int
  }

data ArchetypeData = ArchetypeData
  { id :: ArchetypeId,
    components :: Set ComponentId
  }

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

getArchetypeOnInsert :: ArchetypeId -> [ComponentId] -> ArchetypeGraph -> System ArchetypeData
getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> ArchetypeGraph -> System ArchetypeData