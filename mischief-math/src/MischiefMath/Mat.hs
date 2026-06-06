module MischiefMath.Mat where

import Linear (M33, V3 (V3))

newtype Mat3 = Mat3 (M33 Float) deriving newtype (Show, Eq, Ord, Num)

data Euler = Euler {x :: Float, y :: Float, z :: Float} deriving (Show, Eq)

toEuler :: Mat3 -> Euler
toEuler (Mat3 (V3 (V3 m11 m12 m13) (V3 _ _ m23) (V3 _ _ m33))) =
  Euler {x = atan2 (-m23) m33 * 0.5, y = asin m13 * 0.5, z = atan2 (-m12) m11 * 0.5}
