{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MischiefECS.Components.Bundle where

import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Components
import MischiefECS.Components.Internal
import MischiefECS.Entities.Internal

newtype ProcessedBundleData = ProcessedBundleData {elements :: [ProcessedBundleElement]}

data ProcessedBundleElement = ProcessedBundleElement {id :: ComponentId, component :: ComponentData}

archetypeOfProcessedBundle :: Archetypes -> ProcessedBundleData -> IO ArchetypeId
archetypeOfProcessedBundle archetypes bundle = getOrAddArchetypeId (map (\x -> x.id) bundle.elements) archetypes

addComponentToBundleData :: (Component c) => c -> BundleData -> BundleData
addComponentToBundleData c (BundleData {elements, required}) =
  let rep = ComponentRep $ typeOf c
      component = ErasedComponent c
      element = BundleElement {rep, component}
   in BundleData {elements = Set.insert element elements, required}

instance Eq ProcessedBundleElement where
  (==) :: ProcessedBundleElement -> ProcessedBundleElement -> Bool
  (==) ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = id1 == id2

instance Ord ProcessedBundleElement where
  compare :: ProcessedBundleElement -> ProcessedBundleElement -> Ordering
  compare ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = compare id1 id2

class Bundle b where
  bundleDataInternal :: b -> BundleData

-- | Extracts component IDs from a bundle.
-- getComponentIds :: (Bundle b) => b -> Components -> IO [ComponentId]
-- getComponentIds bundle components =
--  let BundleData set = bundleData bundle
--   in mapM ((`getComponentId` components) . (\x -> x.rep)) (Set.toList set)
instance Bundle () where
  bundleDataInternal :: () -> BundleData
  bundleDataInternal _ = BundleData Set.empty Set.empty

instance Bundle BundleData where
  bundleDataInternal :: BundleData -> BundleData
  bundleDataInternal = id

instance {-# OVERLAPPABLE #-} (Component c, Storage c ~ ComponentStorage) => Bundle (Relationship c) where
  bundleDataInternal :: (Component c, Storage c ~ ComponentStorage) => Relationship c -> BundleData
  bundleDataInternal (R (c, entity)) =
    let DefaultBundleData req = required @c
     in BundleData (Set.fromList [BundleElement {rep = PairRep (typeOf c, entity), component = erase c}]) req

instance {-# OVERLAPPABLE #-} (Component c, Storage c ~ ComponentStorage) => Bundle c where
  bundleDataInternal :: (Component c, Storage c ~ ComponentStorage) => c -> BundleData
  bundleDataInternal c =
    let DefaultBundleData req = required @c
     in BundleData (Set.fromList [BundleElement {rep = ComponentRep $ typeOf c, component = erase c}]) req

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1) => Bundle (c0, c1) where
  bundleDataInternal (c0, c1) =
    let BundleData {elements = set0, required = req0} = bundleDataInternal c0
        BundleData {elements = set1, required = req1} = bundleDataInternal c1
     in BundleData (Set.unions [set0, set1]) (Set.unions [req0, req1])

bundleData :: (Bundle b) => b -> BundleData
bundleData = bundleDataInternal

bundleDataRes :: forall r. (Component r) => r -> BundleData
bundleDataRes r =
  let DefaultBundleData req = required @r
   in BundleData (Set.fromList [BundleElement {rep = ComponentRep $ typeOf r, component = erase r}]) req
