{-# OPTIONS_GHC -Wno-orphans #-}

module Mischief.Math.Vec.Orphans.Default where

import Data.Default
import Linear

instance (Default a) => Default (V2 a) where
  def = V2 (def @a) (def @a)

instance (Default a) => Default (V3 a) where
  def = V3 (def @a) (def @a) (def @a)
