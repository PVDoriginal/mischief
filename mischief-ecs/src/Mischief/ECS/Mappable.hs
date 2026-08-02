{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Mappable where

import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Tables

class Mappable flag a b | flag a -> b where
  mapTuple :: a -> b

class TryMapId flag flag' c out | flag flag' c -> out where
  tryMapId :: c -> out

data RelQueryOutput out = RelQueryOutput out | InvalidRelQueryOutput

type family MapIsId c where
  MapIsId (Maybe a) = False
  MapIsId (Result a) = False
  MapIsId (RelQueryOutput a) = False
  MapIsId [a] = False
  MapIsId a = True

data MapQueryVal

instance TryMapId True MapQueryVal a a where
  tryMapId = id

instance TryMapId False MapQueryVal (Result a) a where
  tryMapId = value

instance (TryMapId (MapIsId a) MapQueryVal a out) => TryMapId False MapQueryVal (Maybe a) (Maybe out) where
  tryMapId = fmap (tryMapId @(MapIsId a) @MapQueryVal)

instance (TryMapId (MapIsId a) MapQueryVal a out) => TryMapId False MapQueryVal [a] [out] where
  tryMapId = map (tryMapId @(MapIsId a) @MapQueryVal)

instance (TryMapId (MapIsId a) MapQueryVal a out) => TryMapId False MapQueryVal (RelQueryOutput a) (RelQueryOutput out) where
  tryMapId (RelQueryOutput a) = RelQueryOutput $ tryMapId @(MapIsId a) @MapQueryVal a
  tryMapId InvalidRelQueryOutput = InvalidRelQueryOutput

instance {-# OVERLAPPABLE #-} (TryMapId (MapIsId a) flag a out) => Mappable flag a out where
  mapTuple = tryMapId @(MapIsId a) @flag

data MapQueryValidity

instance TryMapId True MapQueryValidity a a where
  tryMapId = id

instance TryMapId False MapQueryValidity (Result a) (Result a) where
  tryMapId = id

instance (TryMapId (MapIsId a) MapQueryValidity a out) => TryMapId False MapQueryValidity (RelQueryOutput a) a where
  tryMapId (RelQueryOutput a) = a
  tryMapId InvalidRelQueryOutput = undefined

instance (TryMapId (MapIsId a) MapQueryValidity a out) => TryMapId False MapQueryValidity (Maybe a) (Maybe out) where
  tryMapId = fmap (tryMapId @(MapIsId a) @MapQueryValidity)

instance (TryMapId (MapIsId a) MapQueryValidity a out) => TryMapId False MapQueryValidity [a] [out] where
  tryMapId = map (tryMapId @(MapIsId a) @MapQueryValidity)

-- instance {-# OVERLAPPABLE #-} (TryMapId (MapIsId a) MapQueryValidity a out) => Mappable flag a out where
--   mapTuple = tryMapId @(MapIsId a) @MapQueryValidity

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
