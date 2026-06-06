{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components
  ( Components (Components, map),
    ComponentId (ComponentId),
    emptyComponents,
    getOrAddComponentId,
    getComponentId,
    tryGetComponent,
    Archetypes (Archetypes, map),
    ArchetypeId (ArchetypeId),
    emptyArchetypes,
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
  )
where

import Data.IORef
import Data.List
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Components.Internal

newtype ComponentId = ComponentId
  { id :: Int
  }
  deriving (Show, Eq, Ord)

data Components = Components {map :: IORef (Map TypeRep ComponentId), counter :: IORef Int}

emptyComponents :: IO Components
emptyComponents = do
  map <- newIORef Map.empty
  counter <- newIORef 0
  return $ Components map counter

getOrAddComponentId :: TypeRep -> Components -> IO ComponentId
getOrAddComponentId t Components {map, counter} = do
  innerMap <- readIORef map

  case Map.lookup t innerMap of
    Just t -> return t
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef map $ Map.insert t (ComponentId result)
      return $ ComponentId result

getComponentId :: TypeRep -> Components -> IO (Maybe ComponentId)
getComponentId t Components {map} = do
  innerMap <- readIORef map

  return (Just =<< Map.lookup t innerMap)

tryGetComponent :: forall c. (Component c) => ErasedComponent -> Maybe c
tryGetComponent (ErasedComponent (s :: c')) =
  case eqT @c @c' of
    Just Refl -> Just s
    Nothing -> Nothing

newtype ArchetypeId = ArchetypeId
  { id :: Int
  }
  deriving (Show, Eq, Ord)

data Archetypes = Archetypes {map :: IORef (Map [ComponentId] ArchetypeId), counter :: IORef Int}

emptyArchetypes :: IO Archetypes
emptyArchetypes = do
  map <- newIORef Map.empty
  counter <- newIORef 0
  return $ Archetypes map counter

getOrAddArchetypeId :: [ComponentId] -> Archetypes -> IO ArchetypeId
getOrAddArchetypeId t Archetypes {map, counter} = do
  innerMap <- readIORef map

  case Map.lookup t innerMap of
    Just t -> return t
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef map $ Map.insert t (ArchetypeId result)

      return $ ArchetypeId result

-- | Get the archetype ID from a list of component IDs.
getArchetypeId :: [ComponentId] -> Archetypes -> IO (Maybe ArchetypeId)
getArchetypeId t Archetypes {map} = do
  innerMap <- readIORef map
  return (Just =<< Map.lookup t innerMap)

removeArchetypeId :: ArchetypeId -> Archetypes -> IO ()
removeArchetypeId id archetypes =
  modifyIORef' archetypes.map (Map.filter (/= id))

findMatchingArchetypes :: [ComponentId] -> Archetypes -> IO [([ComponentId], ArchetypeId)]
findMatchingArchetypes components archetypes =
  do
    archetypes <- readIORef archetypes.map
    let archetypesList = Map.toList archetypes
    return $ List.filter (\(archetype, _) -> sort components `isSubsequenceOf` sort archetype) archetypesList

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
