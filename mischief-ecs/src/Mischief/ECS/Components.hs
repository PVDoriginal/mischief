{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Components
  ( -- * Component
    Component (..),
    ComponentId (..),
    Exclusivity (..),

    -- * Meta
    ComponentArchetypes (..),
    ComponentPairs (..),
    DefaultValue (..),
    Requires (..),
    RequiredBy (..),
    ComponentType (..),

    -- * Erasure
    ErasedComponent (..),
    tryGetComponent,
    DefaultComponentType (..),

    -- * Archetypes
    ArchetypeId (..),

    -- * Bundles
    BundleData (..),
    BundleElement (..),

    -- * Tables
    ComponentTicks (..),
    ComponentData (..),

    -- * Storage
    Components (..),
    emptyComponents,
    getComponentId,

    -- * Utils
    Pair (..),
    Rel (..),
    ComponentRep (..),
    Tick (..),
    ErasedComponentEq (..),
    IsExclusive (..),
  )
where

import Data.Default
import Data.IORef
import Data.Kind
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import GHC.Generics
import Mischief.ECS.Collectable
import Mischief.ECS.Components.HooksDef
import Mischief.ECS.EntityDef
import Mischief.ECS.Utils

data Exclusivity = Inclusive | Exclusive

-- | The @Component@ typeclass.
class (Typeable c, IsExclusive (RelExclusivity c)) => Component c where
  -- | List of components required by this one.
  -- All required components must be 'Default'
  --
  -- Example
  --
  -- @
  -- data A = A
  -- instance 'Component' A where
  --   'required' = 'Mischief.ECS.Components.Required.require' @(B, C)
  --
  -- data B = B 'Int' deriving ('Component', 'Generic', 'Default')
  --
  -- data C = C 'String' deriving ('Component')
  -- instance 'Default' C where
  --   'def' = C "Default String"
  -- @
  required :: Set DefaultComponentType
  required = Set.empty

  type RelExclusivity c :: Exclusivity
  type RelExclusivity c = Inclusive

  hooks :: Hooks c
  hooks = Hooks []

class IsExclusive (e :: Exclusivity) where
  isExclusive :: Bool

instance IsExclusive Inclusive where
  isExclusive = False

instance IsExclusive Exclusive where
  isExclusive = True

-- | Unique id for components and component pairs.
data ComponentId = ComponentId
  { -- | The component's entity.
    id :: Entity,
    -- | Optional target entity in case this is a pair / relationship.
    entity :: Maybe Entity
  }
  deriving (Show, Eq, Ord)

newtype Pair = Pair (ComponentType, Entity)

-- | Contains data and methods for assigning 'ComponentId's to new components (via their 'TypeRep').
newtype Components = Components
  { -- | Maps 'TypeRep's to Ints, to be used as the first half of a 'ComponentId'.
    components :: IORef (Map TypeRep Entity)
  }

-- | @Meta@ component with a set of all archetypes that a components is part of.
newtype ComponentArchetypes = ComponentArchetypes {inner :: Set ArchetypeId}
  deriving anyclass (Component)
  deriving newtype (Default)
  deriving stock (Show)

-- | @Meta@ component with a set of all archetypes containing pairs made with this component.
data ComponentPairs = ComponentPairs
  { -- | Archetypes that contain any pair formed with this component.
    any :: Set ArchetypeId,
    -- | Specific archetypes between this component and a particular entity.
    pairs :: Map Entity (Set ArchetypeId)
  }
  deriving anyclass (Component, Default)
  deriving stock (Show, Generic)

-- | Construct an empty 'Components'.
emptyComponents :: IO Components
emptyComponents = do
  components <- newIORef Map.empty
  return $ Components components

-- | Get the id of a component through IO.
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

instance {-# OVERLAPPING #-} EraseIntoStorage () (BundleData ErasedComponent) where
  erase _ = BundleData Set.empty

instance {-# OVERLAPPING #-} EraseIntoStorage (BundleData ErasedComponent) (BundleData ErasedComponent) where
  erase = id

instance (Component c) => EraseIntoStorage c (BundleData ErasedComponent) where
  erase c =
    BundleData $ Set.singleton BundleElement {rep = ComponentRep $ ComponentType $ Proxy @c, component = ErasedComponent c}

instance {-# OVERLAPPING #-} (Component c) => EraseIntoStorage (Rel c) (BundleData ErasedComponent) where
  erase (Rel c entity) =
    BundleData $ Set.singleton BundleElement {rep = PairRep (ComponentType $ Proxy @c, entity), component = ErasedComponent c}

instance (Component c, Eq c) => EraseIntoStorage c (BundleData ErasedComponentEq) where
  erase c =
    BundleData $ Set.singleton BundleElement {rep = ComponentRep $ ComponentType $ Proxy @c, component = ErasedComponentEq c}

instance {-# OVERLAPPING #-} (Component c, Eq c) => EraseIntoStorage (Rel c) (BundleData ErasedComponentEq) where
  erase (Rel c entity) =
    BundleData $ Set.singleton BundleElement {rep = PairRep (ComponentType $ Proxy @c, entity), component = ErasedComponentEq c}

-- | Unique id corresponding to an archetype.
newtype ArchetypeId = ArchetypeId
  { id :: Int
  }
  deriving (Show, Eq, Ord)

-- | Data extracted from a 'Mischief.ECS.Components.Bundle.Bundle'.
newtype BundleData e = BundleData {elements :: Set (BundleElement e)} deriving newtype (Semigroup)

instance Show (BundleData e) where
  show BundleData {elements} = mconcat ["BundleData e [", List.intercalate ", " ts, "]"]
    where
      ts = map (\bundle -> show bundle.rep) (Set.toList elements)

-- | Change ticks for a specific component.
data ComponentTicks = ComponentTicks {changed :: Tick, added :: Tick} deriving (Show)

-- | Data for a component that's stored in a table.
data ComponentData = ComponentData {value :: ErasedComponent, ticks :: ComponentTicks}

-- | Type used for querying and inserting relationships.
data Rel c = Rel {comp :: c, target :: Entity} deriving (Show)

-- | @Meta@ component with the /erased/ default value of this component. Added to components required by other components.
newtype DefaultValue = DefaultValue ErasedComponent deriving anyclass (Component)

instance Component ComponentType where
  required = Set.fromList [DefaultComponentType $ Proxy @ComponentArchetypes, DefaultComponentType $ Proxy @ComponentPairs]

-- | @Meta@ relationship.
data RequiredBy = RequiredBy deriving (Component)

-- | @Meta@ relationship.
data Requires = Requires deriving (Component)

-- | Type for component erasure.
data ErasedComponent where
  ErasedComponent :: (Component c) => c -> ErasedComponent

data ErasedComponentEq where
  ErasedComponentEq :: (Component c, Eq c) => c -> ErasedComponentEq

data ComponentRep = ComponentRep ComponentType | PairRep (ComponentType, Entity) deriving (Show, Eq, Ord)

-- | Element of a 'BundleData e'.
data BundleElement e = BundleElement {rep :: ComponentRep, component :: e}

instance Show (BundleElement a) where
  show :: BundleElement a -> String
  show e = show e.rep

instance Eq (BundleElement a) where
  (==) :: BundleElement a -> BundleElement a -> Bool
  (==) BundleElement {rep = rep1} BundleElement {rep = rep2} = rep1 == rep2

instance Ord (BundleElement a) where
  compare :: BundleElement a -> BundleElement a -> Ordering
  compare BundleElement {rep = rep1} BundleElement {rep = rep2} = compare rep1 rep2

-- @Meta@ component containing the erased type of this component.
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

instance GetRep ComponentType where
  getRep :: ComponentType -> TypeRep
  getRep (ComponentType (_ :: (Proxy t))) = typeRep $ Proxy @t

instance GetRep ErasedComponent where
  getRep :: ErasedComponent -> TypeRep
  getRep (ErasedComponent (_ :: c)) = typeRep $ Proxy @c

instance GetRep DefaultComponentType where
  getRep :: DefaultComponentType -> TypeRep
  getRep (DefaultComponentType (_ :: (Proxy t))) = typeRep $ Proxy @t

newtype Tick = Tick (Int, Int)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Default)