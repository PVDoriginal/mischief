{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components
  ( Components (..),
    ComponentId (ComponentId),
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
    Relationship (R),
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
import MischiefECS.Components.Internal
import MischiefECS.Entities.Internal

newtype ComponentId = ComponentId
  { id :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

newtype Pair = Pair (TypeRep, Entity) deriving newtype (Eq, Ord)

data Components = Components {components :: IORef (Map TypeRep Int), archetypes :: IORef (Map Int (IORef [ArchetypeId])), counter :: IORef Int}

emptyComponents :: IO Components
emptyComponents = do
  components <- newIORef Map.empty
  archetypes <- newIORef Map.empty
  counter <- newIORef 1
  return $ Components components archetypes counter

getOrAddPairId :: Pair -> Components -> IO ComponentId
getOrAddPairId (Pair (t, entity)) components = do
  ComponentId (id, _) <- getOrAddComponentId t components
  return $ ComponentId (id, entity.id)

getOrAddComponentId :: TypeRep -> Components -> IO ComponentId
getOrAddComponentId t Components {components, archetypes, counter} = do
  innerMap <- readIORef components

  case Map.lookup t innerMap of
    Just t -> return $ ComponentId (t, 0)
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef components $ Map.insert t result

      l <- newIORef []
      modifyIORef archetypes $ Map.insert result l

      return $ ComponentId (result, 0)

getComponentId :: TypeRep -> Components -> IO (Maybe ComponentId)
getComponentId t Components {components} = do
  innerMap <- readIORef components
  return $ case Map.lookup t innerMap of
    Nothing -> Nothing
    Just t -> Just $ ComponentId (t, 0)

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

getOrAddArchetypeId :: [ComponentId] -> Archetypes -> Components -> IO ArchetypeId
getOrAddArchetypeId t Archetypes {map, counter} Components {archetypes} = do
  innerMap <- readIORef map

  case Map.lookup t innerMap of
    Just t -> return t
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef map $ Map.insert t (ArchetypeId result)

      archetypes <- readIORef archetypes
      for_ t $ \t -> do
        case Map.lookup (fst t.id) archetypes of
          Nothing -> undefined
          Just list -> do
            print $ "Adding archetype to " ++ show (fst t.id)
            modifyIORef' list (++ [ArchetypeId result])

      return $ ArchetypeId result

-- | Get the archetype ID from a list of component IDs.
getArchetypeId :: [ComponentId] -> Archetypes -> IO (Maybe ArchetypeId)
getArchetypeId t Archetypes {map} = do
  innerMap <- readIORef map
  return (Just =<< Map.lookup t innerMap)

removeArchetypeId :: ArchetypeId -> Archetypes -> IO ()
removeArchetypeId id archetypes =
  modifyIORef' archetypes.map (Map.filter (/= id))

findMatchingArchetypes :: [ComponentId] -> Archetypes -> Components -> IO [([ComponentId], ArchetypeId)]
findMatchingArchetypes components Archetypes {map = map'} Components {archetypes} =
  do
    archetypes' <- readIORef archetypes
    archetypes'' <- forM components $ \component -> do
      print $ "Looking up " ++ show (fst component.id)
      maybe undefined readIORef (Map.lookup (fst component.id) archetypes')

    map' <- readIORef map'

    case archetypes'' of
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

newtype Relationship c = R (c, Entity)

instance (Component c) => Component (Relationship c)