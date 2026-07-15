{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Mappable where

import MischiefECS.Entities
import MischiefECS.Tables
import Prelude hiding (map)

class Mappable flag a b | flag a -> b where
  mapTuple :: a -> b

data MapQueryVal

instance {-# OVERLAPPABLE #-} Mappable MapQueryVal Entity Entity where
  mapTuple = id

instance {-# OVERLAPPABLE #-} Mappable MapQueryVal Bool Bool where
  mapTuple = id

instance {-# OVERLAPPABLE #-} Mappable MapQueryVal (Maybe (Result a)) (Maybe a) where
  mapTuple = fmap (mapTuple @MapQueryVal)

instance {-# OVERLAPPING #-} Mappable MapQueryVal (Result a) a where
  mapTuple (Result (a, _)) = a

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1) => Mappable flag (a0, a1) (b0, b1) where
  mapTuple (a0, a1) = (mapTuple @flag a0, mapTuple @flag a1)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2) => Mappable flag (a0, a1, a2) (b0, b1, b2) where
  mapTuple (a0, a1, a2) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3) => Mappable flag (a0, a1, a2, a3) (b0, b1, b2, b3) where
  mapTuple (a0, a1, a2, a3) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4) => Mappable flag (a0, a1, a2, a3, a4) (b0, b1, b2, b3, b4) where
  mapTuple (a0, a1, a2, a3, a4) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5) => Mappable flag (a0, a1, a2, a3, a4, a5) (b0, b1, b2, b3, b4, b5) where
  mapTuple (a0, a1, a2, a3, a4, a5) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6) => Mappable flag (a0, a1, a2, a3, a4, a5, a6) (b0, b1, b2, b3, b4, b5, b6) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7) (b0, b1, b2, b3, b4, b5, b6, b7) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7, Mappable flag a8 b8) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7, a8) (b0, b1, b2, b3, b4, b5, b6, b7, b8) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7, a8) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7, mapTuple @flag a8)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7, Mappable flag a8 b8, Mappable flag a9 b9) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7, mapTuple @flag a8, mapTuple @flag a9)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7, Mappable flag a8 b8, Mappable flag a9 b9, Mappable flag a10 b10) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7, mapTuple @flag a8, mapTuple @flag a9, mapTuple @flag a10)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7, Mappable flag a8 b8, Mappable flag a9 b9, Mappable flag a10 b10, Mappable flag a11 b11) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7, mapTuple @flag a8, mapTuple @flag a9, mapTuple @flag a10, mapTuple @flag a11)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7, Mappable flag a8 b8, Mappable flag a9 b9, Mappable flag a10 b10, Mappable flag a11 b11, Mappable flag a12 b12) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7, mapTuple @flag a8, mapTuple @flag a9, mapTuple @flag a10, mapTuple @flag a11, mapTuple @flag a12)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7, Mappable flag a8 b8, Mappable flag a9 b9, Mappable flag a10 b10, Mappable flag a11 b11, Mappable flag a12 b12, Mappable flag a13 b13) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7, mapTuple @flag a8, mapTuple @flag a9, mapTuple @flag a10, mapTuple @flag a11, mapTuple @flag a12, mapTuple @flag a13)

instance {-# OVERLAPPING #-} (Mappable flag a0 b0, Mappable flag a1 b1, Mappable flag a2 b2, Mappable flag a3 b3, Mappable flag a4 b4, Mappable flag a5 b5, Mappable flag a6 b6, Mappable flag a7 b7, Mappable flag a8 b8, Mappable flag a9 b9, Mappable flag a10 b10, Mappable flag a11 b11, Mappable flag a12 b12, Mappable flag a13 b13, Mappable flag a14 b14) => Mappable flag (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14) where
  mapTuple (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (mapTuple @flag a0, mapTuple @flag a1, mapTuple @flag a2, mapTuple @flag a3, mapTuple @flag a4, mapTuple @flag a5, mapTuple @flag a6, mapTuple @flag a7, mapTuple @flag a8, mapTuple @flag a9, mapTuple @flag a10, mapTuple @flag a11, mapTuple @flag a12, mapTuple @flag a13, mapTuple @flag a14)
