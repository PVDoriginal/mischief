{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components
  ( Components (..),
    ComponentId (..),
    emptyComponents,
    getComponentId,
    tryGetComponent,
    ArchetypeId (ArchetypeId),
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
    ComponentType (..),
    Meta (..),
    getRep,
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
  { id :: Entity,
    entity :: Maybe Entity
  }
  deriving (Show, Eq, Ord)

newtype Pair = Pair (ComponentType, Entity)

-- | Contains data and methods for assigning 'ComponentId's to new components (via their 'TypeRep').
data Components = Components
  { -- | Maps 'TypeRep's to Ints, to be used as the first half of a 'ComponentId'.
    components :: IORef (Map TypeRep Entity),
    -- | Maps each component int (the first half of a 'ComponentId') to the set of archetypes containing that component.
    -- In the case of relationships, this will contain all archetypes which contain any relationship containing that component.
    archetypes :: IORef (Map Entity (IORef (Set ArchetypeId))),
    -- | Maps whole 'ComponentId's to the set of archetypes which contain them.
    -- This is meant to be used to check the archetypes of component - entity relationships.
    pairs :: IORef (Map ComponentId (IORef (Set ArchetypeId))),
    resources :: IORef (Map ComponentId Entity),
    -- | Counter of ints that are assigned as component ids.
    counter :: IORef Int
  }

-- data ArchetypeRecord = ArchetypeRecord
--   { normal :: IORef (Set ArchetypeId),
--     pairs :: IORef (Map Entity (IORef (Set ArchetypeId))),
--   }

-- | Construct an empty 'Components'.
emptyComponents :: IO Components
emptyComponents = do
  components <- newIORef Map.empty
  archetypes <- newIORef Map.empty
  pairs <- newIORef Map.empty
  resources <- newIORef Map.empty
  counter <- newIORef 1
  return $ Components components archetypes pairs resources counter

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

data Meta c = Meta
  deriving (Component, Show, Eq)

instance Component ComponentType