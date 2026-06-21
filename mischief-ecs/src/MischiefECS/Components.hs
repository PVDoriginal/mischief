{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components where

import Control.Monad
import Data.Default
import Data.Foldable
import Data.IORef
import Data.Kind
import Data.List
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import GHC.Generics
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
newtype Components = Components
  { -- | Maps 'TypeRep's to Ints, to be used as the first half of a 'ComponentId'.
    components :: IORef (Map TypeRep Entity)
  }

newtype ComponentArchetypes = ComponentArchetypes {inner :: Set ArchetypeId}
  deriving anyclass (Component)
  deriving newtype (Default)
  deriving stock (Show)

data ComponentPairs = ComponentPairs {any :: Set ArchetypeId, pairs :: Map Entity (Set ArchetypeId)}
  deriving anyclass (Component, Default)
  deriving stock (Show, Generic)

-- | Construct an empty 'Components'.
emptyComponents :: IO Components
emptyComponents = do
  components <- newIORef Map.empty
  return $ Components components

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

class (Typeable c) => Component c where
  erase :: c -> ErasedComponent
  erase = ErasedComponent

  required :: Set DefaultComponentType
  required = Set.empty

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

-- instance (Component c) => Component (R c)

data Meta c = Meta
  deriving (Component, Show, Eq)

newtype DefaultValue = DefaultValue ErasedComponent deriving anyclass (Component)

instance Component ComponentType where
  required = Set.fromList [DefaultComponentType $ Proxy @ComponentArchetypes]

data RequiredBy = RequiredBy deriving (Component)

data Requires = Requires deriving (Component)

data ErasedComponent where
  ErasedComponent :: (Typeable c) => c -> ErasedComponent

data ComponentRep = ComponentRep ComponentType | PairRep (ComponentType, Entity) deriving (Show, Eq, Ord)

data BundleElement = BundleElement {rep :: ComponentRep, component :: ErasedComponent}

instance Show BundleElement where
  show :: BundleElement -> String
  show e = show e.rep

instance Eq BundleElement where
  (==) :: BundleElement -> BundleElement -> Bool
  (==) BundleElement {rep = rep1} BundleElement {rep = rep2} = rep1 == rep2

instance Ord BundleElement where
  compare :: BundleElement -> BundleElement -> Ordering
  compare BundleElement {rep = rep1} BundleElement {rep = rep2} = compare rep1 rep2

data ComponentType where
  ComponentType :: forall (c :: Type). (Component c) => (Proxy c) -> ComponentType

instance Show ComponentType where
  show :: ComponentType -> String
  show x = show $ getRep x

instance Eq ComponentType where
  (==) :: ComponentType -> ComponentType -> Bool
  (==) a b = getRep a == getRep b

instance Ord ComponentType where
  compare :: ComponentType -> ComponentType -> Ordering
  compare a b = compare (getRep a) (getRep b)

data DefaultComponentType where
  DefaultComponentType :: forall (c :: Type). (Component c, Default c) => (Proxy c) -> DefaultComponentType

instance Show DefaultComponentType where
  show :: DefaultComponentType -> String
  show x = show $ getRep x

instance Eq DefaultComponentType where
  (==) :: DefaultComponentType -> DefaultComponentType -> Bool
  (==) a b = getRep a == getRep b

instance Ord DefaultComponentType where
  compare :: DefaultComponentType -> DefaultComponentType -> Ordering
  compare a b = compare (getRep a) (getRep b)

class GetRep t where
  getRep :: t -> TypeRep

instance GetRep ComponentType where
  getRep :: ComponentType -> TypeRep
  getRep (ComponentType (_ :: (Proxy t))) = typeRep $ Proxy @t

instance GetRep ErasedComponent where
  getRep :: ErasedComponent -> TypeRep
  getRep (ErasedComponent (_ :: c)) = typeRep $ Proxy @c

instance GetRep DefaultComponentType where
  getRep :: DefaultComponentType -> TypeRep
  getRep (DefaultComponentType (_ :: (Proxy t))) = typeRep $ Proxy @t
