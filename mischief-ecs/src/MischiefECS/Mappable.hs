{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Mappable where

import Prelude hiding (map)

class Mappable flag a b | flag a -> b where
  map :: a -> b

instance (Mappable flag a0 b0, Mappable flag a1 b1) => Mappable flag (a0, a1) (b0, b1) where
  map (a0, a1) = (map @flag a0, map @flag a1)
