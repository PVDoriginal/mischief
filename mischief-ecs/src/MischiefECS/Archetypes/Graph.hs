module MischiefECS.Archetypes.Graph where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.List
import Data.Map (Map, mapMaybe)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec
import MischiefECS.World
import MischiefECS.World.Query

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

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

getArchetypeOnInsert :: ArchetypeId -> [ComponentId] -> System ArchetypeData
getArchetypeOnInsert archetype components =
  do
    world <- ask
    let Archetypes {graph} = world.archetypes

    let d = ArchetypeData {id = archetype, components = Set.empty}
    f d components graph
  where
    f archetype [] _ = return archetype
    f archetype (component : xs) graph = do
      x <- getArchetype archetype.id (Inserted component) graph
      f x xs graph

getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> System ArchetypeData
getArchetypeOnRemove archetype components =
  do
    world <- ask
    let Archetypes {graph} = world.archetypes

    let d = ArchetypeData {id = archetype, components = Set.empty}
    f d components graph
  where
    f archetype [] _ = return archetype
    f archetype (component : xs) graph = do
      x <- getArchetype archetype.id (Removed component) graph
      f x xs graph

getRequirements :: ComponentId -> System (Set ComponentId)
getRequirements component = do
  x <- get @(R Requires) component.id
  return $ case x of
    Nothing -> Set.empty
    Just x -> Set.fromList $ map (\x -> ComponentId {id = x, entity = Nothing}) x.targets

findMatchingArchetypes :: [ComponentId] -> Components -> Archetypes -> IO [([ComponentId], ArchetypeId)]
findMatchingArchetypes components Components {archetypes} Archetypes {graph} = do
  archetypes' <- liftIO $ readIORef archetypes
  archetypes'' <- forM components $ \component -> do
    liftIO $ maybe undefined readIORef (Map.lookup component.id archetypes')

  -- map' <- readIORef map'

  case map Set.toList archetypes'' of
    [] -> return []
    h : tail -> do
      let archetypes = foldr intersect h tail

      mapM
        ( \(ArchetypeId x) -> do
            x' <- Vec.read graph.nodes 1
            return (Set.toList x'.archetype.components, ArchetypeId x)
        )
        archetypes
