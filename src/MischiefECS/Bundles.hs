module MischiefECS.Bundles where 

import MischiefECS.Components 
import Data.Data
import Data.List

data BundleData = BundleData {
    types :: [TypeRep],
    components :: [ErasedComponent]
}

instance Show BundleData where 
  show BundleData{types} = mconcat ["BundleData [", intercalate ", " ts ,"]"]
    where ts = Prelude.map show types


class Bundle b where 
    bundleData :: b -> BundleData  

instance {-# OVERLAPPABLE #-} (Component c) => Bundle c where 
  bundleData c = BundleData { types = [typeOf c], components = [erase c]} 

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1) => Bundle (c0, c1) where 
  bundleData(c0, c1) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
    in
      BundleData{
        types = concat [types0, types1],
        components = concat [components0, components1]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2) => Bundle (c0, c1, c2) where 
  bundleData(c0, c1, c2) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
    in
      BundleData{
        types = concat [types0, types1, types2],
        components = concat [components0, components1, components2]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3) => Bundle (c0, c1, c2, c3) where 
  bundleData(c0, c1, c2, c3) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
    in
      BundleData{
        types = concat [types0, types1, types2, types3],
        components = concat [components0, components1, components2, components3]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4) => Bundle (c0, c1, c2, c3, c4) where 
  bundleData(c0, c1, c2, c3, c4) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4],
        components = concat [components0, components1, components2, components3, components4]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5) => Bundle (c0, c1, c2, c3, c4, c5) where 
  bundleData(c0, c1, c2, c3, c4, c5) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5],
        components = concat [components0, components1, components2, components3, components4, components5]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6) => Bundle (c0, c1, c2, c3, c4, c5, c6) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6],
        components = concat [components0, components1, components2, components3, components4, components5, components6]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7, c8) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
      BundleData {types = types8, components = components8} = bundleData c8
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7, types8],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7, components8]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
      BundleData {types = types8, components = components8} = bundleData c8
      BundleData {types = types9, components = components9} = bundleData c9
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7, types8, types9],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7, components8, components9]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
      BundleData {types = types8, components = components8} = bundleData c8
      BundleData {types = types9, components = components9} = bundleData c9
      BundleData {types = types10, components = components10} = bundleData c10
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7, types8, types9, types10],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7, components8, components9, components10]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
      BundleData {types = types8, components = components8} = bundleData c8
      BundleData {types = types9, components = components9} = bundleData c9
      BundleData {types = types10, components = components10} = bundleData c10
      BundleData {types = types11, components = components11} = bundleData c11
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7, types8, types9, types10, types11],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7, components8, components9, components10, components11]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11, Bundle c12) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
      BundleData {types = types8, components = components8} = bundleData c8
      BundleData {types = types9, components = components9} = bundleData c9
      BundleData {types = types10, components = components10} = bundleData c10
      BundleData {types = types11, components = components11} = bundleData c11
      BundleData {types = types12, components = components12} = bundleData c12
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7, types8, types9, types10, types11, types12],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7, components8, components9, components10, components11, components12]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11, Bundle c12, Bundle c13) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
      BundleData {types = types8, components = components8} = bundleData c8
      BundleData {types = types9, components = components9} = bundleData c9
      BundleData {types = types10, components = components10} = bundleData c10
      BundleData {types = types11, components = components11} = bundleData c11
      BundleData {types = types12, components = components12} = bundleData c12
      BundleData {types = types13, components = components13} = bundleData c13
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7, types8, types9, types10, types11, types12, types13],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7, components8, components9, components10, components11, components12, components13]
      }

instance {-# OVERLAPPING #-} (Bundle c0, Bundle c1, Bundle c2, Bundle c3, Bundle c4, Bundle c5, Bundle c6, Bundle c7, Bundle c8, Bundle c9, Bundle c10, Bundle c11, Bundle c12, Bundle c13, Bundle c14) => Bundle (c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) where 
  bundleData(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) = 
    let
      BundleData {types = types0, components = components0} = bundleData c0
      BundleData {types = types1, components = components1} = bundleData c1
      BundleData {types = types2, components = components2} = bundleData c2
      BundleData {types = types3, components = components3} = bundleData c3
      BundleData {types = types4, components = components4} = bundleData c4
      BundleData {types = types5, components = components5} = bundleData c5
      BundleData {types = types6, components = components6} = bundleData c6
      BundleData {types = types7, components = components7} = bundleData c7
      BundleData {types = types8, components = components8} = bundleData c8
      BundleData {types = types9, components = components9} = bundleData c9
      BundleData {types = types10, components = components10} = bundleData c10
      BundleData {types = types11, components = components11} = bundleData c11
      BundleData {types = types12, components = components12} = bundleData c12
      BundleData {types = types13, components = components13} = bundleData c13
      BundleData {types = types14, components = components14} = bundleData c14
    in
      BundleData{
        types = concat [types0, types1, types2, types3, types4, types5, types6, types7, types8, types9, types10, types11, types12, types13, types14],
        components = concat [components0, components1, components2, components3, components4, components5, components6, components7, components8, components9, components10, components11, components12, components13, components14]
      }

