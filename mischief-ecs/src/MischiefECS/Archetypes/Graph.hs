module MischiefECS.Archetypes.Graph where

import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec

data ArchetypeGraph = ArchetypeGraph {nodes :: IOVec ArchetypeNode, lookup :: IORef (Map (Set ComponentId) Int), counter :: IORef Int}

newArchetypeGraph :: IO ArchetypeGraph
newArchetypeGraph = do
  nodes <- Vec.new 1024

  -- Add the empty archetype to the graph.
  Vec.pushBack nodes ArchetypeNode {archetype = ArchetypeData {id = ArchetypeId 0, components = Set.empty}, insert = Map.empty, remove = Map.empty}

  counter <- newIORef 1
  lookup <- newIORef $ Map.singleton Set.empty 0
  return $ ArchetypeGraph {nodes, lookup, counter}

getNewId :: ArchetypeGraph -> IO Int
getNewId ArchetypeGraph {counter} = do
  x <- readIORef counter
  modifyIORef' counter (+ 1)
  return x

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

getArchetype :: ArchetypeId -> ArchetypeTransition -> ArchetypeGraph -> IO ArchetypeData
getArchetype (ArchetypeId id) (Removed component) graph = do
  node <- Vec.read graph.nodes id

  undefined
getArchetype (ArchetypeId id) (Inserted component) graph = do
  node <- Vec.read graph.nodes id

  undefined