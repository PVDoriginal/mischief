module MischiefECS.Components.Bundle where

import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Components
import MischiefECS.Components.Internal

newtype ProcessedBundleData = ProcessedBundleData {elements :: [ProcessedBundleElement]}

data ProcessedBundleElement = ProcessedBundleElement {id :: ComponentId, component :: ComponentData}

archetypeOfProcessedBundle :: Archetypes -> ProcessedBundleData -> IO ArchetypeId
archetypeOfProcessedBundle archetypes bundle = getArchetypeId (map (\x -> x.id) bundle.elements) archetypes

addComponentToBundleData :: (Component c) => c -> BundleData -> BundleData
addComponentToBundleData c (BundleData {elements, required}) =
  let rep = typeOf c
      component = ErasedComponent c
      element = BundleElement {rep, component}
   in BundleData {elements = Set.insert element elements, required}

instance Eq ProcessedBundleElement where
  (==) :: ProcessedBundleElement -> ProcessedBundleElement -> Bool
  (==) ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = id1 == id2

instance Ord ProcessedBundleElement where
  compare :: ProcessedBundleElement -> ProcessedBundleElement -> Ordering
  compare ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = compare id1 id2

instance Show BundleData where
  show BundleData {elements, required} = mconcat ["BundleData [", List.intercalate ", " ts, "]"]
    where
      ts = map (\bundle -> show bundle.rep) (Set.toList (Set.union elements required))

class Bundle b where
  bundleDataInternal :: b -> BundleData

-- | Extracts component IDs from a bundle.
-- getComponentIds :: (Bundle b) => b -> Components -> IO [ComponentId]
-- getComponentIds bundle components =
--   let BundleData set = bundleData bundle
--    in mapM ((`getComponentId` components) . (\x -> x.rep)) (Set.toList set)
instance Bundle () where
  bundleDataInternal :: () -> BundleData
  bundleDataInternal _ = BundleData Set.empty Set.empty

instance Bundle BundleData where
  bundleDataInternal :: BundleData -> BundleData
  bundleDataInternal = id

instance {-# OVERLAPPABLE #-} (Component c) => Bundle c where
  bundleDataInternal c =
    let DefaultBundleData req = required @c
     in BundleData (Set.fromList [BundleElement {rep = typeOf c, component = erase c}]) req

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1) => Bundle (c0, c1) where
  bundleDataInternal (c0, c1) =
    let BundleData {elements = set0, required = req0} = bundleDataInternal c0
        BundleData {elements = set1, required = req1} = bundleDataInternal c1
     in BundleData (Set.unions [set0, set1]) (Set.unions [req0, req1])

bundleData :: (Bundle b) => b -> BundleData
bundleData = bundleDataInternal

bundleDataRes :: (Resource r) => r -> BundleData
bundleDataRes r =
  BundleData (Set.fromList [BundleElement {rep = typeOf r, component = erase r}]) Set.empty
