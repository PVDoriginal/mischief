module MischiefECS.Components.BundleTypes where

import Data.Data
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Components

class BundleTypes t where
  types :: Proxy t -> Set TypeRep

instance {-# OVERLAPPABLE #-} (Component c) => BundleTypes c where
  types :: Proxy c -> Set TypeRep
  types = Set.singleton . typeRep

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1) => BundleTypes (t0, t1) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2) => BundleTypes (t0, t1, t2) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3) => BundleTypes (t0, t1, t2, t3) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4) => BundleTypes (t0, t1, t2, t3, t4) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5) => BundleTypes (t0, t1, t2, t3, t4, t5) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6) => BundleTypes (t0, t1, t2, t3, t4, t5, t6) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7, BundleTypes t8) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7, t8) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7, types $ Proxy @t8]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7, BundleTypes t8, BundleTypes t9) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7, types $ Proxy @t8, types $ Proxy @t9]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7, BundleTypes t8, BundleTypes t9, BundleTypes t10) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7, types $ Proxy @t8, types $ Proxy @t9, types $ Proxy @t10]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7, BundleTypes t8, BundleTypes t9, BundleTypes t10, BundleTypes t11) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7, types $ Proxy @t8, types $ Proxy @t9, types $ Proxy @t10, types $ Proxy @t11]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7, BundleTypes t8, BundleTypes t9, BundleTypes t10, BundleTypes t11, BundleTypes t12) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7, types $ Proxy @t8, types $ Proxy @t9, types $ Proxy @t10, types $ Proxy @t11, types $ Proxy @t12]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7, BundleTypes t8, BundleTypes t9, BundleTypes t10, BundleTypes t11, BundleTypes t12, BundleTypes t13) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7, types $ Proxy @t8, types $ Proxy @t9, types $ Proxy @t10, types $ Proxy @t11, types $ Proxy @t12, types $ Proxy @t13]

instance {-# OVERLAPPING #-} (BundleTypes t0, BundleTypes t1, BundleTypes t2, BundleTypes t3, BundleTypes t4, BundleTypes t5, BundleTypes t6, BundleTypes t7, BundleTypes t8, BundleTypes t9, BundleTypes t10, BundleTypes t11, BundleTypes t12, BundleTypes t13, BundleTypes t14) => BundleTypes (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  types _ = Set.unions [types $ Proxy @t0, types $ Proxy @t1, types $ Proxy @t2, types $ Proxy @t3, types $ Proxy @t4, types $ Proxy @t5, types $ Proxy @t6, types $ Proxy @t7, types $ Proxy @t8, types $ Proxy @t9, types $ Proxy @t10, types $ Proxy @t11, types $ Proxy @t12, types $ Proxy @t13, types $ Proxy @t14]
