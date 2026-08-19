module Mischief.Math.Affine where

import Mischief.Math.Mat
import Mischief.Math.Vec (Vec3)

data Affine3 = Affine3 {matrix :: Mat3, translation :: Vec3} deriving (Show, Eq)