{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MischiefECS.Components.Bundle where

import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Components.Internal
import MischiefECS.Components.Required (requireAll, toBundleElement)

newtype ProcessedBundleData = ProcessedBundleData {elements :: [ProcessedBundleElement]}

data ProcessedBundleElement = ProcessedBundleElement {id :: ComponentId, component :: ComponentData}

-- archetypeOfProcessedBundle :: Archetypes -> Components -> ProcessedBundleData -> IO ArchetypeId
-- archetypeOfProcessedBundle archetypes components bundle = getOrAddArchetypeId (map (\x -> x.id) bundle.elements) archetypes components

addComponentToBundleData :: forall c. (Component c) => c -> BundleData -> BundleData
addComponentToBundleData c (BundleData {elements, required}) =
  let rep = ComponentRep $ ComponentType (Proxy @c)
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
instance {-# OVERLAPPING #-} Bundle () where
  bundleDataInternal :: () -> BundleData
  bundleDataInternal _ = BundleData Set.empty Set.empty

instance {-# OVERLAPPING #-} Bundle BundleData where
  bundleDataInternal :: BundleData -> BundleData
  bundleDataInternal = id

instance {-# OVERLAPPING #-} (Component c) => Bundle (R c) where
  bundleDataInternal :: (Component c) => R c -> BundleData
  bundleDataInternal (R (c, entity)) =
    let req = Set.map toBundleElement $ requireAll @c
     in BundleData (Set.fromList [BundleElement {rep = PairRep (ComponentType $ Proxy @c, entity), component = erase c}]) req

instance {-# OVERLAPPABLE #-} (Component c) => Bundle c where
  bundleDataInternal :: (Component c) => c -> BundleData
  bundleDataInternal c =
    let req = Set.map toBundleElement $ requireAll @c
     in BundleData (Set.fromList [BundleElement {rep = ComponentRep $ ComponentType $ Proxy @c, component = erase c}]) req

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1) => Bundle (c0, c1) where
  bundleDataInternal (c0, c1) =
    let BundleData {elements = set0, required = req0} = bundleDataInternal c0
        BundleData {elements = set1, required = req1} = bundleDataInternal c1
     in BundleData (Set.unions [set0, set1]) (Set.unions [req0, req1])

bundleData :: (Bundle b) => b -> BundleData
bundleData = bundleDataInternal

bundleDataRes :: forall r. (Component r) => r -> BundleData
bundleDataRes r =
  let req = Set.map toBundleElement $ requireAll @r
   in BundleData (Set.fromList [BundleElement {rep = ComponentRep $ ComponentType $ Proxy @r, component = erase r}]) req

data BTrue a

data BFalse a

type family Bundleable a where
  Bundleable (Component a) = BTrue a
  Bundleable (RelationLike a) = BTrue a
  Bundleable a = BFalse a

class RelationLike a

instance RelationLike (R a)