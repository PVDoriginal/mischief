{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.Components.Bundle where

import Data.Set qualified as Set
import Data.Typeable
import Mischief.ECS.Collectable (Collectable (collect))
import Mischief.ECS.Components

newtype ProcessedBundleData = ProcessedBundleData {elements :: [ProcessedBundleElement]}

data ProcessedBundleElement = ProcessedBundleElement {id :: ComponentId, component :: ComponentData}

addComponentToBundleData :: forall c. (Component c) => c -> BundleData -> BundleData
addComponentToBundleData c (BundleData {elements}) =
  let rep = ComponentRep $ ComponentType (Proxy @c)
      component = ErasedComponent c
      element = BundleElement {rep, component}
   in BundleData {elements = Set.union elements (Set.singleton element)}

instance Eq ProcessedBundleElement where
  (==) :: ProcessedBundleElement -> ProcessedBundleElement -> Bool
  (==) ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = id1 == id2

instance Ord ProcessedBundleElement where
  compare :: ProcessedBundleElement -> ProcessedBundleElement -> Ordering
  compare ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = compare id1 id2

type Bundle b = Collectable b BundleData

bundleData :: (Bundle b) => b -> BundleData
bundleData = collect
