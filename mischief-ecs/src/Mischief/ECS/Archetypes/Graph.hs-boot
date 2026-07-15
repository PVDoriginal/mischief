module Mischief.ECS.Archetypes.Graph where

import Control.Monad.Primitive
import Mischief.ECS.Archetypes
import Mischief.ECS.Components
import Mischief.ECS.World

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

data ComponentQuery = ComponentQuery | RelationshipQueryAny | RelationshipQuery

getArchetypeOnInsert :: ArchetypeId -> [ComponentId] -> System ArchetypeData
getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> System ArchetypeData
findMatchingArchetypes :: forall m w. (MonadSystem w m) => [(ComponentId, ComponentQuery)] -> Archetypes -> m [([ComponentId], ArchetypeId)]
getArchetypeOnSpawn :: [ComponentId] -> System ArchetypeData