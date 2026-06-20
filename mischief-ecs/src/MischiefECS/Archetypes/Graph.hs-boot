module MischiefECS.Archetypes.Graph where

import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.World

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

getArchetypeOnInsert :: ArchetypeId -> [ComponentId] -> System ArchetypeData
getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> System ArchetypeData
findMatchingArchetypes :: [ComponentId] -> Components -> Archetypes -> IO [([ComponentId], ArchetypeId)]