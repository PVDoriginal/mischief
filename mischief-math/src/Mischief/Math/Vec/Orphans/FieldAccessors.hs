{-# OPTIONS_GHC -Wno-orphans #-}

module Mischief.Math.Vec.Orphans.FieldAccessors where

import GHC.Records
import Linear

instance HasField "x" (V2 a) a where
  getField :: V2 a -> a
  getField (V2 x _) = x

instance HasField "y" (V2 a) a where
  getField :: V2 a -> a
  getField (V2 _ y) = y

instance HasField "x" (V3 a) a where
  getField :: V3 a -> a
  getField (V3 x _ _) = x

instance HasField "y" (V3 a) a where
  getField :: V3 a -> a
  getField (V3 _ y _) = y

instance HasField "z" (V3 a) a where
  getField :: V3 a -> a
  getField (V3 _ _ z) = z