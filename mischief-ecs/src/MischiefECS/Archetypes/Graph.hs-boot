module MischiefECS.Archetypes.Graph where

import Control.Monad.Primitive
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.World
import MischiefECS.World.Internal

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

data ComponentQuery = ComponentQuery | RelationshipQueryAny | RelationshipQuery

getArchetypeOnInsert :: ArchetypeId -> [ComponentId] -> System ArchetypeData
getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> System ArchetypeData
findMatchingArchetypes :: forall m w. (MonadSystem w m) => [(ComponentId, ComponentQuery)] -> Archetypes -> m [([ComponentId], ArchetypeId)]