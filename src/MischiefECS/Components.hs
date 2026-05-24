{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components
  ( Components (Components, map),
    ComponentId (ComponentId),
    emptyComponents,
    getComponentId,
    tryGetComponent,
    Archetypes (Archetypes),
    ArchetypeId (ArchetypeId),
    emptyArchetypes,
    getArchetypeId,
    removeArchetypeId,
    findMatchingArchetypes,
    Component (erase, required),
    ErasedComponent,
    BundleData (BundleData, elements, required),
    BundleElement (BundleElement, rep, component),
  )
where

import Data.IORef
import Data.List
import Data.List qualified as List
import Data.Map
import Data.Map qualified as Map
import Data.Set
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

getComponentId :: TypeRep -> Components -> IO ComponentId
getComponentId t Components {map, counter} = do
  innerMap <- readIORef map

  case Data.Map.lookup t innerMap of
    Just t -> return t
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef map $ Data.Map.insert t (ComponentId result)

      return $ ComponentId result

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

-- | Get the archetype ID from a list of component IDs.
getArchetypeId :: [ComponentId] -> Archetypes -> IO ArchetypeId
getArchetypeId t Archetypes {map, counter} = do
  innerMap <- readIORef map

  case Data.Map.lookup t innerMap of
    Just t -> return t
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef map $ Data.Map.insert t (ArchetypeId result)

      return $ ArchetypeId result

removeArchetypeId :: ArchetypeId -> Archetypes -> IO ()
removeArchetypeId id archetypes = do
  modifyIORef' archetypes.map (Map.filter (/= id))

findMatchingArchetypes :: [ComponentId] -> Archetypes -> IO [([ComponentId], ArchetypeId)]
findMatchingArchetypes components archetypes =
  do
    archetypes <- readIORef archetypes.map
    let archetypesList = Map.toList archetypes
    return $ List.filter (\(archetype, _) -> sort components `isSubsequenceOf` sort archetype) archetypesList

class (Typeable c) => Component c where
  erase :: c -> ErasedComponent
  erase = ErasedComponent

  required :: DefaultBundleData
  required = DefaultBundleData Set.empty

data BundleData = BundleData {elements :: Set BundleElement, required :: Set BundleElement}

instance Eq BundleElement where
  (==) :: BundleElement -> BundleElement -> Bool
  (==) BundleElement {rep = rep1} BundleElement {rep = rep2} = rep1 == rep2

instance Ord BundleElement where
  compare :: BundleElement -> BundleElement -> Ordering
  compare BundleElement {rep = rep1} BundleElement {rep = rep2} = compare rep1 rep2
