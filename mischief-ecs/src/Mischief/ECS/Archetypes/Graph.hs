module Mischief.ECS.Archetypes.Graph where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.List
import Data.Map (Map, mapMaybe)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Base (eqWord#, isTrue#)
import Mischief.ECS.Archetypes
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.EntityDef
import Mischief.ECS.Log
import Mischief.ECS.Relationships
import Mischief.ECS.Tables
import Mischief.ECS.Utils
import Mischief.ECS.Vec (IOVec)
import Mischief.ECS.Vec qualified as Vec
import Mischief.ECS.World
import Mischief.ECS.World (SystemTools (get))
import Mischief.ECS.World.Query.Queryable

data ArchetypeTransition = Inserted ComponentId | Removed ComponentId

getNewId :: ArchetypeGraph -> IO Int
getNewId ArchetypeGraph {counter} = do
  x <- readIORef counter
  modifyIORef' counter (+ 1)
  return x

createNode :: Set ComponentId -> System Int
createNode components = do
  world <- unsafeGetWorld
  let Archetypes {graph} = world.archetypes

  id <- liftIO $ getNewId graph
  liftIO $ modifyIORef' graph.lookup $ Map.insert components id
  Vec.pushBack graph.nodes ArchetypeNode {archetype = ArchetypeData {id = ArchetypeId id, components}, insert = Map.empty, remove = Map.empty}

  comps <-
    mapM
      ( \(ComponentId (# id, _ #)) -> do
          t <- worldGet (Proxy @ComponentType) (Entity (# id, 0## #))
          return $ fmap getRep t
      )
      (Set.toList components)

  -- debug $ "archetype " <> text id <> " = " <> text (catMaybes comps)

  for_ components $ \(ComponentId (# id', target' #)) -> do
    case target' of
      -- Component isn't a pair.
      Nothing -> do
        set <- worldGet (Proxy @ComponentArchetypes) (Entity (# id', 0## #))
        for_ set $ \set -> do
          worldSet (ComponentArchetypes {inner = Set.insert (ArchetypeId id) set.inner}) (Entity (# id', 0## #))
      -- modify set $ \ComponentArchetypes {inner} -> ComponentArchetypes {inner = Set.insert (ArchetypeId id) inner}
      -- Component is a pair.
      Just e -> do
        set <- worldGet (Proxy @ComponentPairs) (Entity (# id', 0## #))
        for_ set $ \set -> do
          let ComponentPairs {any, pairs} = set
          flip worldSet (Entity (# id', 0## #)) $
            ComponentPairs
              { any = Set.insert (ArchetypeId id) any,
                pairs =
                  Map.alter
                    ( \case
                        Nothing -> Just $ Set.singleton $ ArchetypeId id
                        Just s -> Just $ Set.insert (ArchetypeId id) s
                    )
                    e
                    pairs
              }

  return id

getOrCreateNode :: Set ComponentId -> System Int
getOrCreateNode components = do
  world <- unsafeGetWorld
  let Archetypes {graph} = world.archetypes

  lookup <- liftIO $ readIORef graph.lookup
  case Map.lookup components lookup of
    Just x -> return x
    Nothing -> createNode components

addEdge :: Int -> Int -> ComponentId -> System ()
addEdge a b component = do
  world <- unsafeGetWorld
  let Archetypes {graph} = world.archetypes
  Vec.modify_ graph.nodes a $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert = Map.insert component b insert, remove, archetype}
  Vec.modify_ graph.nodes b $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert, remove = Map.insert component a remove, archetype}

addEdgeI :: Int -> Int -> ComponentId -> System ()
addEdgeI a b component = do
  world <- unsafeGetWorld
  let Archetypes {graph} = world.archetypes
  Vec.modify_ graph.nodes a $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert = Map.insert component b insert, remove, archetype}

addEdgeR :: Int -> Int -> ComponentId -> System ()
addEdgeR a b component = do
  world <- unsafeGetWorld
  let Archetypes {graph} = world.archetypes
  Vec.modify_ graph.nodes b $ \ArchetypeNode {insert, remove, archetype} -> ArchetypeNode {insert, remove = Map.insert component a remove, archetype}

getArchetypeOnRemoveSingle :: ArchetypeId -> ComponentId -> System ArchetypeData
getArchetypeOnRemoveSingle (ArchetypeId id) component = do
  world <- unsafeGetWorld
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

getArchetypeOnInsertSingle :: ArchetypeId -> ComponentId -> System ArchetypeData
getArchetypeOnInsertSingle (ArchetypeId id) component = do
  world <- unsafeGetWorld
  let Archetypes {graph} = world.archetypes

  node <- Vec.read graph.nodes id

  case Map.lookup component node.insert of
    Just x -> do
      newNode <- Vec.read graph.nodes x
      return newNode.archetype
    Nothing -> do
      components <- do
        let components = node.archetype.components

        let !(ComponentId (# id, _ #)) = component

        isExclusiveRel <- isJust <$> worldGet (Proxy @IsExclusiveRelationship) (Entity (# id, 0## #))

        case isExclusiveRel of
          True ->
            return $ Set.filter (\(ComponentId (# id', a #)) -> not (isTrue# $ eqWord# id' id) || isNothing a) components
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
    world <- unsafeGetWorld
    let Archetypes {graph} = world.archetypes
    let d = ArchetypeData {id = archetype, components = Set.empty}

    f d components graph
  where
    f archetype [] _ = return archetype
    f archetype (component : xs) graph = do
      x <- getArchetypeOnInsertSingle archetype.id component
      f x xs graph

newtype ArchetypeRemovalResult = ArchetypeRemovalResult {removed :: [ComponentId]}

getArchetypeOnRemove :: ArchetypeId -> [ComponentId] -> System (ArchetypeData, [ComponentId])
getArchetypeOnRemove archetype components =
  do
    world <- unsafeGetWorld
    let Archetypes {graph} = world.archetypes
    let d = ArchetypeData {id = archetype, components = Set.empty}

    f d components graph
  where
    f archetype [] _ = return (archetype, [])
    f archetype (component : xs) graph = do
      x <- getArchetypeOnRemoveSingle archetype.id component

      (a, b) <- f x xs graph
      if x.id /= archetype.id
        then return (a, b ++ [component])
        else return (a, b)

getArchetypeOnSpawn :: [ComponentId] -> System ArchetypeData
getArchetypeOnSpawn components =
  do
    world <- unsafeGetWorld
    let Archetypes {graph} = world.archetypes

    regs <- mapM getRequirements components
    let allComps = foldr' (flip Set.union) (Set.fromList components) regs
    node <- getOrCreateNode allComps

    nodeData <- Vec.read graph.nodes node

    return nodeData.archetype

getRequirements :: ComponentId -> System (Set ComponentId)
getRequirements (ComponentId (# id, _ #)) = do
  x <- worldGetRAny (Proxy @Requires) (Entity (# id, 0## #))
  return $ case x of
    Nothing -> Set.empty
    Just x -> Set.fromList $ map ((\(Entity (# id, _ #)) -> ComponentId (# id, Nothing #)) . (\x -> x.target)) x

data ComponentQuery = ComponentQuery | RelationshipQueryAny | RelationshipQuery

findMatchingArchetypes :: forall m w. (MonadSystem w m) => [(ComponentId, ComponentQuery)] -> Archetypes -> m [([ComponentId], ArchetypeId)]
findMatchingArchetypes [] _ = allArchetypes
findMatchingArchetypes components Archetypes {graph} = do
  archetypes'' <- forM components $ \(ComponentId (# id, target #), q) -> do
    case q of
      ComponentQuery -> do
        Just x <- worldGet (Proxy @ComponentArchetypes) (Entity (# id, 0## #))
        return x.inner
      RelationshipQueryAny -> do
        Just x <- worldGet (Proxy @ComponentPairs) (Entity (# id, 0## #))
        return x.any
      RelationshipQuery -> do
        case target of
          Nothing -> undefined
          Just target -> do
            Just x <- worldGet (Proxy @ComponentPairs) (Entity (# id, 0## #))
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

allArchetypes :: forall m w. (MonadSystem w m) => m [([ComponentId], ArchetypeId)]
allArchetypes = do
  world <- unsafeGetWorld
  map (\x -> (Set.toList x.archetype.components, x.archetype.id)) <$> Vec.toList world.archetypes.graph.nodes
