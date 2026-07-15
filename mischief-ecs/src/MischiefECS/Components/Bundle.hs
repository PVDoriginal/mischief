{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MischiefECS.Components.Bundle where

import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Archetypes
import MischiefECS.Collectable (Collectable (collect), EraseIntoStorage, Idk)
import MischiefECS.Components
import MischiefECS.Components.Required (requireAll, toBundleElement)

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

-- class Bundle b where
--   bundleData :: b -> BundleData

-- instance {-# OVERLAPPING #-} Bundle () where
--   bundleData :: () -> BundleData
--   bundleData _ = BundleData Set.empty

-- instance {-# OVERLAPPING #-} Bundle BundleData where
--   bundleData :: BundleData -> BundleData
--   bundleData = id

-- instance {-# OVERLAPPING #-} (Component c) => Bundle (Rel c) where
--   bundleData :: (Component c) => Rel c -> BundleData
--   bundleData (Rel (c, entity)) =
--     BundleData (Set.fromList [BundleElement {rep = PairRep (ComponentType $ Proxy @c, entity), component = erase c}])

-- instance {-# OVERLAPPABLE #-} (Component c) => Bundle c where
--   bundleData :: (Component c) => c -> BundleData
--   bundleData c =
--     BundleData (Set.fromList [BundleElement {rep = ComponentRep $ ComponentType $ Proxy @c, component = erase c}])

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1) => Bundle (c0, c1) where
--   bundleData (c0, c1) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--      in BundleData $ Set.unions [set0, set1]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2) => Bundle (c0, c1, c2) where
--   bundleData (c0, c1, c2) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--      in BundleData $ Set.unions [set0, set1, set2]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3) => Bundle (c0, c1, c2, c3) where
--   bundleData (c0, c1, c2, c3) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--      in BundleData $ Set.unions [set0, set1, set2, set3]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4) => Bundle (c0, c1, c2, c3, c4) where
--   bundleData (c0, c1, c2, c3, c4) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5) => Bundle (c0, c1, c2, c3, c4, c5) where
--   bundleData (c0, c1, c2, c3, c4, c5) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6) => Bundle (c0, c1, c2, c3, c4, c5, c6) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7, c8) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--         BundleData {elements = set8} = bundleData c8
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7, set8]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--         BundleData {elements = set8} = bundleData c8
--         BundleData {elements = set9} = bundleData c9
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7, set8, set9]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--         BundleData {elements = set8} = bundleData c8
--         BundleData {elements = set9} = bundleData c9
--         BundleData {elements = set10} = bundleData c10
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7, set8, set9, set10]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--         BundleData {elements = set8} = bundleData c8
--         BundleData {elements = set9} = bundleData c9
--         BundleData {elements = set10} = bundleData c10
--         BundleData {elements = set11} = bundleData c11
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7, set8, set9, set10, set11]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11, Bundle c12) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--         BundleData {elements = set8} = bundleData c8
--         BundleData {elements = set9} = bundleData c9
--         BundleData {elements = set10} = bundleData c10
--         BundleData {elements = set11} = bundleData c11
--         BundleData {elements = set12} = bundleData c12
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7, set8, set9, set10, set11, set12]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11, Bundle c12, Bundle c13) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--         BundleData {elements = set8} = bundleData c8
--         BundleData {elements = set9} = bundleData c9
--         BundleData {elements = set10} = bundleData c10
--         BundleData {elements = set11} = bundleData c11
--         BundleData {elements = set12} = bundleData c12
--         BundleData {elements = set13} = bundleData c13
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7, set8, set9, set10, set11, set12, set13]

-- instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11, Bundle c12, Bundle c13, Bundle c14) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) where
--   bundleData (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) =
--     let BundleData {elements = set0} = bundleData c0
--         BundleData {elements = set1} = bundleData c1
--         BundleData {elements = set2} = bundleData c2
--         BundleData {elements = set3} = bundleData c3
--         BundleData {elements = set4} = bundleData c4
--         BundleData {elements = set5} = bundleData c5
--         BundleData {elements = set6} = bundleData c6
--         BundleData {elements = set7} = bundleData c7
--         BundleData {elements = set8} = bundleData c8
--         BundleData {elements = set9} = bundleData c9
--         BundleData {elements = set10} = bundleData c10
--         BundleData {elements = set11} = bundleData c11
--         BundleData {elements = set12} = bundleData c12
--         BundleData {elements = set13} = bundleData c13
--         BundleData {elements = set14} = bundleData c14
--      in BundleData $ Set.unions [set0, set1, set2, set3, set4, set5, set6, set7, set8, set9, set10, set11, set12, set13, set14]

-- bundleDataRes :: forall r. (Component r) => r -> BundleData
-- bundleDataRes r =
--   BundleData (Set.fromList [BundleElement {rep = ComponentRep $ ComponentType $ Proxy @r, component = erase r}])
