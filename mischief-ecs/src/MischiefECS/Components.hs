{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components
  ( -- * Component
    Component (..),
    ComponentId (..),

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
import {-# SOURCE #-} MischiefECS.Entities
import MischiefECS.Utils

-- | The @Component@ typeclass.
class (Typeable c) => Component c where
  -- | Automatic type erasure
  erase :: c -> ErasedComponent
  erase = ErasedComponent

  -- | List of components required by this one.
  -- All required components must be 'Default'
  --
  -- Example
  --
  -- @
  -- data A = A
  -- instance 'Component' A where
  --   'required' = 'MischiefECS.Components.Required.require' @(B, C)
  --
  -- data B = B 'Int' deriving ('Component', 'Generic', 'Default')
  --
  -- data C = C 'String' deriving ('Component')
  -- instance 'Default' C where
  --   'def' = C "Default String"
  -- @
  required :: Set DefaultComponentType
  required = Set.empty

  -- | Set to 'True' in order to make relationships containing this components @exclusive@.
  isExclusiveRel :: Bool
  isExclusiveRel = False

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

-- | @Meta@ components with a set of all archetypes that a components is part of.
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

-- | Unique id corresponding to an archetype.
newtype ArchetypeId = ArchetypeId
  { id :: Int
  }
  deriving (Show, Eq, Ord)

-- | Data extracted from a 'MischiefECS.Components.Bundle.Bundle'.
newtype BundleData = BundleData {elements :: Set BundleElement}

instance Show BundleData where
  show BundleData {elements} = mconcat ["BundleData [", List.intercalate ", " ts, "]"]
    where
      ts = map (\bundle -> show bundle.rep) (Set.toList elements)

-- | Change ticks for a specific component.
data ComponentTicks = ComponentTicks {changed :: Tick, added :: Tick} deriving (Show)

-- | Data for a component that's stored in a table.
data ComponentData = ComponentData {value :: ErasedComponent, ticks :: ComponentTicks}

instance Component Entity

-- | Type used for querying and inserting relationships.
newtype Rel c = Rel (c, Entity)

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
  ErasedComponent :: (Typeable c) => c -> ErasedComponent

data ComponentRep = ComponentRep ComponentType | PairRep (ComponentType, Entity) deriving (Show, Eq, Ord)

-- | Element of a 'BundleData'.
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
