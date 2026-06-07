module MischiefMath.Vec
  ( V2,
    V3,
    Vec2,
    Vec3,
    vec2,
    vec3,
    extend,
    trunc,
    Dir3 (Dir3),
    dir3,
  )
where

import GHC.Records
import Linear

type Vec2 = V2 Float

type Vec3 = V3 Float

vec2 :: (Float, Float) -> Vec2
vec2 (x, y) = V2 x y

vec3 :: (Float, Float, Float) -> Vec3
vec3 (x, y, z) = V3 x y z

extend :: a -> V2 a -> V3 a
extend z (V2 x y) = V3 x y z

trunc :: V3 a -> V2 a
trunc (V3 x y _) = V2 x y

-- TODO: make a custom instance for Num and Functor to ensure Dir3 stays normalized
newtype Dir3 = Dir3 (V3 Float) deriving newtype (Show, Eq, Ord, Num)

instance HasField "vec" Dir3 Vec3 where
  getField :: Dir3 -> Vec3
  getField (Dir3 v3) = v3

dir3 :: (Float, Float, Float) -> Dir3
dir3 (x, y, z) = Dir3 $ signorm $ V3 x y z