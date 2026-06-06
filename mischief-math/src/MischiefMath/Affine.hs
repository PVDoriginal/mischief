module MischiefMath.Affine where

import MischiefMath.Mat
import MischiefMath.Vec (Vec3)

data Affine3 = Affine3 {matrix :: Mat3, translation :: Vec3} deriving (Show, Eq)