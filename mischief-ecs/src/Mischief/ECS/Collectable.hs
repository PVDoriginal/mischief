module Mischief.ECS.Collectable where

class Idk x y

class (Semigroup s) => EraseIntoStorage v s where
  erase :: v -> s

class Collectable v storage where
  collect :: v -> storage

instance (EraseIntoStorage v storage) => Collectable v storage where
  collect = erase

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s) => Collectable (a0, a1) s where
  collect (a0, a1) = foldr (<>) (collect a0) [collect a1]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s) => Collectable (a0, a1, a2) s where
  collect (a0, a1, a2) = foldr (<>) (collect a0) [collect a1, collect a2]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s) => Collectable (a0, a1, a2, a3) s where
  collect (a0, a1, a2, a3) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s) => Collectable (a0, a1, a2, a3, a4) s where
  collect (a0, a1, a2, a3, a4) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s) => Collectable (a0, a1, a2, a3, a4, a5) s where
  collect (a0, a1, a2, a3, a4, a5) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s) => Collectable (a0, a1, a2, a3, a4, a5, a6) s where
  collect (a0, a1, a2, a3, a4, a5, a6) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s, Collectable a8 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7, a8) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7, a8) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7, collect a8]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s, Collectable a8 s, Collectable a9 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7, collect a8, collect a9]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s, Collectable a8 s, Collectable a9 s, Collectable a10 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7, collect a8, collect a9, collect a10]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s, Collectable a8 s, Collectable a9 s, Collectable a10 s, Collectable a11 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7, collect a8, collect a9, collect a10, collect a11]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s, Collectable a8 s, Collectable a9 s, Collectable a10 s, Collectable a11 s, Collectable a12 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7, collect a8, collect a9, collect a10, collect a11, collect a12]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s, Collectable a8 s, Collectable a9 s, Collectable a10 s, Collectable a11 s, Collectable a12 s, Collectable a13 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7, collect a8, collect a9, collect a10, collect a11, collect a12, collect a13]

instance {-# OVERLAPPING #-} (Semigroup s, Collectable a0 s, Collectable a1 s, Collectable a2 s, Collectable a3 s, Collectable a4 s, Collectable a5 s, Collectable a6 s, Collectable a7 s, Collectable a8 s, Collectable a9 s, Collectable a10 s, Collectable a11 s, Collectable a12 s, Collectable a13 s, Collectable a14 s) => Collectable (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) s where
  collect (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = foldr (<>) (collect a0) [collect a1, collect a2, collect a3, collect a4, collect a5, collect a6, collect a7, collect a8, collect a9, collect a10, collect a11, collect a12, collect a13, collect a14]
