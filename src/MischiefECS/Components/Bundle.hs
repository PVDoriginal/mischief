module MischiefECS.Components.Bundle where

import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Components

newtype ProcessedBundleData = ProcessedBundleData {elements :: [ProcessedBundleElement]}

data ProcessedBundleElement = ProcessedBundleElement {id :: ComponentId, component :: ErasedComponent}

data RawBundleData = RawBundleData {elements :: Set BundleElement, required :: Set BundleElement}

archetypeOfProcessedBundle :: Archetypes -> ProcessedBundleData -> IO ArchetypeId
archetypeOfProcessedBundle archetypes bundle = getArchetypeId (map (\x -> x.id) bundle.elements) archetypes

addComponentToBundleData :: (Component c) => c -> BundleData -> BundleData
addComponentToBundleData c (BundleData bundle) =
  let rep = typeOf c
      component = ErasedComponent c
      element = BundleElement {rep, component}
   in BundleData $ Set.insert element bundle

instance Eq ProcessedBundleElement where
  (==) :: ProcessedBundleElement -> ProcessedBundleElement -> Bool
  (==) ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = id1 == id2

instance Ord ProcessedBundleElement where
  compare :: ProcessedBundleElement -> ProcessedBundleElement -> Ordering
  compare ProcessedBundleElement {id = id1} ProcessedBundleElement {id = id2} = compare id1 id2

instance Show BundleData where
  show (BundleData set) = mconcat ["BundleData [", List.intercalate ", " ts, "]"]
    where
      ts = map (\bundle -> show bundle.rep) (Set.toList set)

class Bundle b where
  bundleDataInternal :: b -> RawBundleData

-- | Extracts component IDs from a bundle.
getComponentIds :: (Bundle b) => b -> Components -> IO [ComponentId]
getComponentIds bundle components =
  let BundleData set = bundleData bundle
   in mapM ((`getComponentId` components) . (\x -> x.rep)) (Set.toList set)

instance Bundle () where
  bundleDataInternal :: () -> RawBundleData
  bundleDataInternal _ = RawBundleData Set.empty Set.empty

instance {-# OVERLAPPABLE #-} (Component c) => Bundle c where
  bundleDataInternal c =
    let DefaultBundleData req = required @c
     in RawBundleData (Set.fromList [BundleElement {rep = typeOf c, component = erase c}]) req

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1) => Bundle (c0, c1) where
  bundleDataInternal (c0, c1) =
    let RawBundleData {elements = set0, required = req0} = bundleDataInternal c0
        RawBundleData {elements = set1, required = req1} = bundleDataInternal c1
     in RawBundleData (Set.unions [set0, set1]) (Set.unions [req0, req1])

mergeRawBundleData :: RawBundleData -> BundleData
mergeRawBundleData RawBundleData {elements, required} = BundleData $ Set.union elements required

bundleData :: (Bundle b) => b -> BundleData
bundleData b = mergeRawBundleData $ bundleDataInternal b
