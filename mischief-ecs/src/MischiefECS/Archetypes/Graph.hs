module MischiefECS.Archetypes.Graph where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List
import Data.Map (Map, mapMaybe)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Relationships
import MischiefECS.Tables
import MischiefECS.Utils
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec
import MischiefECS.World
import MischiefECS.World.Defer
import MischiefECS.World.Modify
import MischiefECS.World.Query

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

getNewId :: ArchetypeGraph -> IO Int
getNewId ArchetypeGraph {counter} = do
  x <- readIORef counter
  modifyIORef' counter (+ 1)
  return x

createNode :: Set ComponentId -> System Int
createNode components = do
  world <- ask
  let Archetypes {graph} = world.archetypes

  id <- liftIO $ getNewId graph
  liftIO $ modifyIORef' graph.lookup $ Map.insert components id
  Vec.pushBack graph.nodes ArchetypeNode {archetype = ArchetypeData {id = ArchetypeId id, components}, insert = Map.empty, remove = Map.empty}

  -- archetypes <- liftIO $ readIORef world.components.archetypes

  comps <-
    mapM
      ( \c -> do
          t <- get @ComponentType @System c.id
          return $ fmap (getRep . value) t
      )
      (Set.toList components)

  liftIO $ putStrLn $ "archetype " ++ show id ++ " = " ++ show (catMaybes comps)
  for_ components $ \component -> do
    case component.entity of
      -- Component isn't a pair.
      Nothing -> do
        set <- get @ComponentArchetypes component.id
        for_ set $ \set -> do
          modify set $ \ComponentArchetypes {inner} -> ComponentArchetypes {inner = Set.insert (ArchetypeId id) inner}
      -- Component is a pair.
      Just entity -> do
        set <- get @ComponentPairs component.id
        for_ set $ \set -> do
          modify set $ \ComponentPairs {any, pairs} ->
            ComponentPairs
              { any = Set.insert (ArchetypeId id) any,
                pairs =
                  Map.alter
                    ( \case
                        Nothing -> Just $ Set.singleton $ ArchetypeId id
                        Just s -> Just $ Set.insert (ArchetypeId id) s
                    )
                    entity
                    pairs
              }

  return id

getOrCreateNode :: Set ComponentId -> System Int
getOrCreateNode components = do
  world <- ask
  let Archetypes {graph} = world.archetypes

  lookup <- liftIO $ readIORef graph.lookup
  case Map.lookup components lookup of
    Just x -> return x
    Nothing -> createNode components

addEdge :: Int -> Int -> ComponentId -> System ()
addEdge a b component = do
  world <- ask
  let Archetypes {graph} = world.archetypes
  Vec.modify_ graph.nodes a $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert = Map.insert component b insert, remove, archetype}
  Vec.modify_ graph.nodes b $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert, remove = Map.insert component a remove, archetype}

addEdgeI :: Int -> Int -> ComponentId -> System ()
addEdgeI a b component = do
  world <- ask
  let Archetypes {graph} = world.archetypes
  Vec.modify_ graph.nodes a $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert = Map.insert component b insert, remove, archetype}

addEdgeR :: Int -> Int -> ComponentId -> System ()
addEdgeR a b component = do
  world <- ask
  let Archetypes {graph} = world.archetypes
  Vec.modify_ graph.nodes b $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert, remove = Map.insert component a remove, archetype}

getArchetype :: ArchetypeId -> ArchetypeTransition -> System ArchetypeData
getArchetype (ArchetypeId id) (Removed component) = do
  world <- ask
  let Archetypes {graph} = world.archetypes

  node <- Vec.read graph.nodes id

  case Map.lookup component node.remove of
    Just x -> do
      newNode <- Vec.read graph.nodes x
      return newNode.archetype
    Nothing -> do
      let components = node.archetype.components
      -- TODO: check if another component requires this one!

      let newComponents = Set.delete component components
      newId <- getOrCreateNode newComponents
      addEdgeR newId id component

      newNode <- Vec.read graph.nodes newId
      return newNode.archetype
getArchetype (ArchetypeId id) (Inserted component) = do
  world <- ask
  let Archetypes {graph} = world.archetypes

  node <- Vec.read graph.nodes id

  case Map.lookup component node.insert of
    Just x -> do
      newNode <- Vec.read graph.nodes x
      return newNode.archetype
    Nothing -> do
      components <- do
        let components = node.archetype.components

        isExclusiveRel <- get @(Has IsExclusiveRelationship) component.id

        case isExclusiveRel of
          Just True ->
            return $ Set.filter (\c -> c.id /= component.id || isNothing c.entity) components
          _ ->
            return components

      requirements <- getRequirements component

      let newComponents = Set.union (Set.insert component components) requirements

      newId <- getOrCreateNode newComponents
      addEdgeI id newId component

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
      x <- getArchetype archetype.id (Inserted component)
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
      x <- getArchetype archetype.id (Removed component)
      f x xs graph

getArchetypeOnSpawn :: [ComponentId] -> System ArchetypeData
getArchetypeOnSpawn components =
  do
    world <- ask
    let Archetypes {graph} = world.archetypes

    regs <- mapM getRequirements components
    let allComps = foldr' (flip Set.union) (Set.fromList components) regs
    node <- getOrCreateNode allComps

    nodeData <- Vec.read graph.nodes node

    return nodeData.archetype

getRequirements :: ComponentId -> System (Set ComponentId)
getRequirements component = do
  x <- get @(Rel Requires) component.id
  return $ case x of
    Nothing -> Set.empty
    Just x -> Set.fromList $ map ((\x -> ComponentId {id = x, entity = Nothing}) . target) x

data ComponentQuery = ComponentQuery | RelationshipQueryAny | RelationshipQuery

findMatchingArchetypes :: forall m w. (MonadSystem w m) => [(ComponentId, ComponentQuery)] -> Archetypes -> m [([ComponentId], ArchetypeId)]
findMatchingArchetypes components Archetypes {graph} = do
  archetypes'' <- forM components $ \(component, q) -> do
    case q of
      ComponentQuery -> do
        Just x <- get @ComponentArchetypes component.id
        return x.inner
      RelationshipQueryAny -> do
        Just x <- get @ComponentPairs component.id
        return x.any
      RelationshipQuery -> do
        let target = fromMaybe undefined component.entity
        Just x <- get @ComponentPairs component.id
        return $ fromMaybe undefined $ Map.lookup target x.pairs

  case map Set.toList archetypes'' of
    [] -> return []
    h : tail -> do
      let archetypes = foldr intersect h tail

      mapM
        ( \(ArchetypeId x) -> do
            x' <- Vec.read graph.nodes x
            return (Set.toList x'.archetype.components, ArchetypeId x)
        )
        archetypes
