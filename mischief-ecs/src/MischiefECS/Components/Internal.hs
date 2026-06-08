module MischiefECS.Components.Internal where

import Data.Data
import Data.Set
import MischiefECS.Entities.Internal

newtype DefaultBundleData = DefaultBundleData (Set BundleElement)

data ErasedComponent where
  ErasedComponent :: (Typeable c) => c -> ErasedComponent

data ComponentRep = ComponentRep TypeRep | PairRep (TypeRep, Entity) deriving (Show, Eq, Ord)

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
