{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components
  ( Components (..),
    ComponentId (..),
    emptyComponents,
    getOrAddComponentId,
    getComponentId,
    tryGetComponent,
    Archetypes (Archetypes, map),
    ArchetypeId (ArchetypeId),
    emptyArchetypes,
    getOrAddPairId,
    getOrAddArchetypeId,
    getArchetypeId,
    removeArchetypeId,
    findMatchingArchetypes,
    Component (Storage, erase, required),
    ErasedComponent,
    BundleData (BundleData, elements, required),
    BundleElement (BundleElement, rep, component),
    Tick (Tick),
    ComponentTicks (ComponentTicks, changed, added),
    ComponentData (ComponentData, value, ticks),
    StorageType (ComponentStorage, ResourceStorage),
    Pair (..),
    R (..),
  )
where

import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import GHC.Records
import MischiefECS.Components.Internal
import MischiefECS.Entities.Internal

-- | Unique ids for components and component pairs.
data ComponentId = ComponentId
  { id :: Int,
    entity :: Maybe Entity
  }
  deriving (Show, Eq, Ord)

newtype Pair = Pair (TypeRep, Entity) deriving newtype (Eq, Ord)

-- | Contains data and methods for assigning 'ComponentId's to new components (via their 'TypeRep').
data Components = Components
  { -- | Maps 'TypeRep's to Ints, to be used as the first half of a 'ComponentId'.
    components :: IORef (Map TypeRep Int),
    -- | Maps each component int (the first half of a 'ComponentId') to the set of archetypes containing that component.
    -- In the case of relationships, this will contain all archetypes which contain any relationship containing that component.
    archetypes :: IORef (Map Int (IORef (Set ArchetypeId))),
    -- | Maps whole 'ComponentId's to the set of archetypes which contain them.
    -- This is meant to be used to check the archetypes of component - entity relationships.
    pairs :: IORef (Map ComponentId (IORef (Set ArchetypeId))),
    -- | Counter of ints that are assigned as component ids.
    counter :: IORef Int
  }

-- | Construct an empty 'Components'.
emptyComponents :: IO Components
emptyComponents = do
  components <- newIORef Map.empty
  archetypes <- newIORef Map.empty
  pairs <- newIORef Map.empty
  counter <- newIORef 1
  return $ Components components archetypes pairs counter

-- | Get the id of a component - entity pair. In case the component isn't registered, it will give it a new id.
getOrAddPairId :: Pair -> Components -> IO ComponentId
getOrAddPairId (Pair (t, entity)) components = do
  component <- getOrAddComponentId t components
  return $ ComponentId {id = component.id, entity = Just entity}

-- | Get the id of a component. In case the component isn't registered, it will give it a new id.
getOrAddComponentId :: TypeRep -> Components -> IO ComponentId
getOrAddComponentId t Components {components, archetypes, counter} = do
  innerMap <- readIORef components

  case Map.lookup t innerMap of
    Just t -> return $ ComponentId {id = t, entity = Nothing}
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef components $ Map.insert t result

      l <- newIORef Set.empty
      modifyIORef archetypes $ Map.insert result l

      return $ ComponentId {id = result, entity = Nothing}

-- | Get the id of a component.
getComponentId :: TypeRep -> Components -> IO (Maybe ComponentId)
getComponentId t Components {components} = do
  innerMap <- readIORef components
  return $ case Map.lookup t innerMap of
    Nothing -> Nothing
    Just t -> Just ComponentId {id = t, entity = Nothing}

-- | Try to get the inner data of a 'ErasedComponent'.
tryGetComponent :: forall c. (Component c) => ErasedComponent -> Maybe c
tryGetComponent (ErasedComponent (s :: c')) =
  case eqT @c @c' of
    Just Refl -> Just s
    Nothing -> Nothing

-- | Unique id corresponding to an archetype.
newtype ArchetypeId = ArchetypeId
  { id :: Int
  }
  deriving (Show, Eq, Ord)

data Archetypes = Archetypes {map :: IORef (Map [ComponentId] ArchetypeId), revMap :: IORef (Map ArchetypeId [ComponentId]), counter :: IORef Int}

-- Construct an empty 'Archetypes'.
emptyArchetypes :: IO Archetypes
emptyArchetypes = do
  map <- newIORef Map.empty
  map' <- newIORef Map.empty
  counter <- newIORef 0
  return $ Archetypes map map' counter

-- | Get the archetype id for a list of component ids. In case this archetype doesn't exist, it will create it.
--
-- This will also update the maps in 'Components' which map component ids to archetypes that contain them.
getOrAddArchetypeId :: [ComponentId] -> Archetypes -> Components -> IO ArchetypeId
getOrAddArchetypeId t Archetypes {map, revMap, counter} Components {archetypes, pairs} = do
  innerMap <- readIORef map

  case Map.lookup t innerMap of
    Just t -> return t
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef map $ Map.insert t (ArchetypeId result)
      modifyIORef revMap $ Map.insert (ArchetypeId result) t

      archetypes <- readIORef archetypes
      pairs' <- readIORef pairs
      for_ t $ \t -> do
        case Map.lookup t.id archetypes of
          Nothing -> undefined
          Just list -> modifyIORef' list $ Set.insert $ ArchetypeId result

        when (isJust t.entity) $ do
          case Map.lookup t pairs' of
            Nothing -> do
              s <- newIORef $ Set.singleton $ ArchetypeId result
              modifyIORef' pairs $ Map.insert t s
            Just list -> do
              modifyIORef' list $ Set.insert $ ArchetypeId result

      return $ ArchetypeId result

-- | Get the archetype ID from a list of component IDs.
getArchetypeId :: [ComponentId] -> Archetypes -> IO (Maybe ArchetypeId)
getArchetypeId t Archetypes {map} = do
  innerMap <- readIORef map
  return (Just =<< Map.lookup t innerMap)

removeArchetypeId :: ArchetypeId -> Archetypes -> Components -> IO ()
removeArchetypeId id Archetypes {map, revMap} Components {archetypes, pairs} = do
  revMap' <- readIORef revMap
  case Map.lookup id revMap' of
    Nothing -> undefined
    Just componentList -> do
      modifyIORef' map $ Map.delete componentList

      archetypes <- readIORef archetypes
      pairs <- readIORef pairs
      for_ componentList $ \component -> do
        case Map.lookup component.id archetypes of
          Nothing -> undefined
          Just l -> modifyIORef' l $ Set.delete id

        when (isJust component.entity) $
          case Map.lookup component pairs of
            Nothing -> undefined
            Just l -> modifyIORef' l $ Set.delete id

  modifyIORef' revMap $ Map.delete id

findMatchingArchetypes :: [ComponentId] -> Archetypes -> Components -> IO [([ComponentId], ArchetypeId)]
findMatchingArchetypes components Archetypes {map = map'} Components {archetypes} =
  do
    archetypes' <- readIORef archetypes
    archetypes'' <- forM components $ \component -> do
      maybe undefined readIORef (Map.lookup component.id archetypes')

    map' <- readIORef map'

    case map Set.toList archetypes'' of
      [] -> return []
      h : tail -> do
        let archetypes = foldr intersect h tail
        return $
          mapMaybe
            (\archetype -> find (\(_, id) -> id == archetype) $ Map.toList map')
            archetypes

data StorageType = ComponentStorage | ResourceStorage

class (Typeable c) => Component c where
  type Storage c :: StorageType
  type Storage c = ComponentStorage

  erase :: c -> ErasedComponent
  erase = ErasedComponent

  required :: DefaultBundleData
  required = DefaultBundleData Set.empty

data BundleData = BundleData {elements :: Set BundleElement, required :: Set BundleElement}

instance Show BundleData where
  show BundleData {elements, required} = mconcat ["BundleData [", List.intercalate ", " ts, "]"]
    where
      ts = map (\bundle -> show bundle.rep) (Set.toList (Set.union elements required))

newtype Tick = Tick Int deriving (Show, Eq, Ord)

data ComponentTicks = ComponentTicks {changed :: Tick, added :: Tick} deriving (Show)

data ComponentData = ComponentData {value :: ErasedComponent, ticks :: ComponentTicks}

instance Component Entity

newtype R c = R (c, Entity)

instance (Component c) => Component (R c)
