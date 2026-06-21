module MischiefECS.Archetypes.Graph where

import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.World
import MischiefECS.World.Internal

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

getArchetypeOnInsert :: ArchetypeId -> [ComponentId] -> System ArchetypeData
getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> System ArchetypeData
findMatchingArchetypes :: forall m w. (MonadSystem w m) => [ComponentId] -> Archetypes -> m [([ComponentId], ArchetypeId)]