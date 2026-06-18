module MischiefECS.Components.Internal where

import Data.Data
import Data.Kind
import Data.Set
import MischiefECS.Entities.Internal

newtype DefaultBundleData = DefaultBundleData (Set BundleElement)

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
  ComponentType :: forall (c :: Type). (Typeable c) => (Proxy c) -> ComponentType

instance Show ComponentType where
  show :: ComponentType -> String
  show x = show $ getRep x

instance Eq ComponentType where
  (==) :: ComponentType -> ComponentType -> Bool
  (==) a b = getRep a == getRep b

instance Ord ComponentType where
  compare :: ComponentType -> ComponentType -> Ordering
  compare a b = compare (getRep a) (getRep b)

getRep :: ComponentType -> TypeRep
getRep (ComponentType (_ :: (Proxy t))) = typeRep $ Proxy @t

getErasedRep :: ErasedComponent -> TypeRep
getErasedRep (ErasedComponent (_ :: c)) = typeRep $ Proxy @c