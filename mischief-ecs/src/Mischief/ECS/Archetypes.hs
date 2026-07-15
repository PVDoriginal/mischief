module Mischief.ECS.Archetypes where

import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Mischief.ECS.Components
import Mischief.ECS.Vec
import Mischief.ECS.Vec qualified as Vec

newtype Archetypes = Archetypes {graph :: ArchetypeGraph}

data ArchetypeGraph = ArchetypeGraph {nodes :: IOVec ArchetypeNode, lookup :: IORef (Map (Set ComponentId) Int), counter :: IORef Int}

newArchetypeGraph :: IO ArchetypeGraph
newArchetypeGraph = do
  nodes <- Vec.new 1024

  -- Add the empty archetype to the graph.
  Vec.pushBack nodes ArchetypeNode {archetype = ArchetypeData {id = ArchetypeId 0, components = Set.empty}, insert = Map.empty, remove = Map.empty}

  counter <- newIORef 1
  lookup <- newIORef $ Map.singleton Set.empty 0
  return $ ArchetypeGraph {nodes, lookup, counter}

data ArchetypeNode = ArchetypeNode
  { archetype :: ArchetypeData,
    insert :: Map ComponentId Int,
    remove :: Map ComponentId Int
  }

data ArchetypeData = ArchetypeData
  { id :: ArchetypeId,
    components :: Set ComponentId
  }

-- Construct an empty 'Archetypes'.
emptyArchetypes :: IO Archetypes
emptyArchetypes =
  Archetypes <$> newArchetypeGraph
