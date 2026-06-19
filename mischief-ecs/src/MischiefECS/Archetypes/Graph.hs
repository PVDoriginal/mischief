module MischiefECS.Archetypes.Graph where

import Control.Monad.IO.Class
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec
import MischiefECS.World
import MischiefECS.World.Query

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

createNode :: Set ComponentId -> ArchetypeGraph -> IO Int
createNode components graph = do
  id <- getNewId graph
  modifyIORef' graph.lookup $ Map.insert components id
  Vec.pushBack graph.nodes ArchetypeNode {archetype = ArchetypeData {id = ArchetypeId id, components}, insert = Map.empty, remove = Map.empty}
  return id

getOrCreateNode :: Set ComponentId -> ArchetypeGraph -> IO Int
getOrCreateNode components graph = do
  lookup <- readIORef graph.lookup
  case Map.lookup components lookup of
    Just x -> return x
    Nothing -> createNode components graph

addEdge :: Int -> Int -> ComponentId -> ArchetypeGraph -> IO ()
addEdge a b component graph = do
  Vec.modify_ graph.nodes a $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert = Map.insert component b insert, remove, archetype}
  Vec.modify_ graph.nodes b $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert, remove = Map.insert component b remove, archetype}

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

getArchetype :: ArchetypeId -> ArchetypeTransition -> ArchetypeGraph -> System ArchetypeData
getArchetype (ArchetypeId id) (Removed component) graph = do
  node <- Vec.read graph.nodes id

  case Map.lookup component node.remove of
    Just x -> do
      newNode <- Vec.read graph.nodes x
      return newNode.archetype
    Nothing -> do
      let components = node.archetype.components
      -- TODO: check if another component requires this one!

      let newComponents = Set.delete component components
      newId <- liftIO $ getOrCreateNode newComponents graph
      liftIO $ addEdge newId id component graph

      newNode <- Vec.read graph.nodes newId
      return newNode.archetype
getArchetype (ArchetypeId id) (Inserted component) graph = do
  node <- Vec.read graph.nodes id

  case Map.lookup component node.insert of
    Just x -> do
      newNode <- Vec.read graph.nodes x
      return newNode.archetype
    Nothing -> do
      let components = node.archetype.components
      requirements <- getRequirements component

      let newComponents = Set.union requirements $ Set.insert component components

      newId <- liftIO $ getOrCreateNode newComponents graph
      liftIO $ addEdge id newId component graph

      newNode <- Vec.read graph.nodes newId
      return newNode.archetype

getArchetypeOnInsert :: ArchetypeId -> [ComponentId] -> ArchetypeGraph -> System ArchetypeData
getArchetypeOnInsert archetype components graph =
  do
    let d = ArchetypeData {id = archetype, components = Set.empty}
    f d components
  where
    f archetype [] = return archetype
    f archetype (component : xs) = do
      x <- getArchetype archetype.id (Inserted component) graph
      f x xs

getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> ArchetypeGraph -> System ArchetypeData
getArchetypeOnRemove archetype components graph =
  do
    let d = ArchetypeData {id = archetype, components = Set.empty}
    f d components
  where
    f archetype [] = return archetype
    f archetype (component : xs) = do
      x <- getArchetype archetype.id (Removed component) graph
      f x xs

getRequirements :: ComponentId -> System (Set ComponentId)
getRequirements component = do
  Just x <- get @(R Requires) component.id
  return $ Set.fromList $ map (\x -> ComponentId {id = x, entity = Nothing}) x.targets