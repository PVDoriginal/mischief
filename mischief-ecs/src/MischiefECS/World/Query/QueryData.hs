module MischiefECS.World.Query.QueryData where

import Data.Data
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Components

data TypeQuery = CompQ | RelQ deriving (Eq, Ord)

data MaybeRel a = MaybeRel

data Has a = Has

data HasRel a = HasRel

class QueryData qd where
  types :: Proxy qd -> Set (TypeRep, TypeQuery)

instance {-# OVERLAPPABLE #-} (Component c) => QueryData c where
  types :: (Component c) => Proxy c -> Set (TypeRep, TypeQuery)
  types c = Set.singleton (typeRep c, CompQ)

instance {-# OVERLAPPING #-} (Component c) => QueryData (Rel c) where
  types :: Proxy (Rel c) -> Set (TypeRep, TypeQuery)
  types _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance {-# OVERLAPPING #-} (Component c) => QueryData (Maybe c) where
  types :: (Component c) => Proxy (Maybe c) -> Set (TypeRep, TypeQuery)
  types _ = Set.empty

instance {-# OVERLAPPING #-} (Component c) => QueryData (MaybeRel c) where
  types :: (Component c) => Proxy (MaybeRel c) -> Set (TypeRep, TypeQuery)
  types _ = Set.empty

instance {-# OVERLAPPING #-} (Component c) => QueryData (Has c) where
  types :: (Component c) => Proxy (Has c) -> Set (TypeRep, TypeQuery)
  types _ = Set.empty

instance {-# OVERLAPPING #-} (Component c) => QueryData (HasRel c) where
  types :: (Component c) => Proxy (HasRel c) -> Set (TypeRep, TypeQuery)
  types _ = Set.empty

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1) => QueryData (a0, a1) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2) => QueryData (a0, a1, a2) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3) => QueryData (a0, a1, a2, a3) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4) => QueryData (a0, a1, a2, a3, a4) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5) => QueryData (a0, a1, a2, a3, a4, a5) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6) => QueryData (a0, a1, a2, a3, a4, a5, a6) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7, QueryData a8) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7, a8) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7, types $ Proxy @a8]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7, QueryData a8, QueryData a9) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7, types $ Proxy @a8, types $ Proxy @a9]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7, QueryData a8, QueryData a9, QueryData a10) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7, types $ Proxy @a8, types $ Proxy @a9, types $ Proxy @a10]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7, QueryData a8, QueryData a9, QueryData a10, QueryData a11) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7, types $ Proxy @a8, types $ Proxy @a9, types $ Proxy @a10, types $ Proxy @a11]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7, QueryData a8, QueryData a9, QueryData a10, QueryData a11, QueryData a12) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7, types $ Proxy @a8, types $ Proxy @a9, types $ Proxy @a10, types $ Proxy @a11, types $ Proxy @a12]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7, QueryData a8, QueryData a9, QueryData a10, QueryData a11, QueryData a12, QueryData a13) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7, types $ Proxy @a8, types $ Proxy @a9, types $ Proxy @a10, types $ Proxy @a11, types $ Proxy @a12, types $ Proxy @a13]

instance {-# OVERLAPPING #-} (QueryData a0, QueryData a1, QueryData a2, QueryData a3, QueryData a4, QueryData a5, QueryData a6, QueryData a7, QueryData a8, QueryData a9, QueryData a10, QueryData a11, QueryData a12, QueryData a13, QueryData a14) => QueryData (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2, types $ Proxy @a3, types $ Proxy @a4, types $ Proxy @a5, types $ Proxy @a6, types $ Proxy @a7, types $ Proxy @a8, types $ Proxy @a9, types $ Proxy @a10, types $ Proxy @a11, types $ Proxy @a12, types $ Proxy @a13, types $ Proxy @a14]
