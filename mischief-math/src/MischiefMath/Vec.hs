module MischiefMath.Vec
  ( Vec2 (inner),
    Vec3 (inner),
    vec2,
    vec3,
    extend,
    trunc,
    Dir3 (inner),
    dir3,
  )
where

import Data.Default (Default (def))
import GHC.Records
import Linear (V2 (V2), V3 (V3), signorm)

newtype Vec2 = Vec2 {inner :: V2 Float} deriving newtype (Show, Eq, Ord, Num)

newtype Vec3 = Vec3 {inner :: V3 Float} deriving newtype (Show, Eq, Ord, Num)

vec2 :: (Float, Float) -> Vec2
vec2 (x, y) = Vec2 $ V2 x y

vec3 :: (Float, Float, Float) -> Vec3
vec3 (x, y, z) = Vec3 $ V3 x y z

instance Default Vec2 where
  def = vec2 (0, 0)

instance Default Vec3 where
  def = vec3 (0, 0, 0)

instance HasField "x" Vec2 Float where
  getField :: Vec2 -> Float
  getField (Vec2 (V2 x _)) = x

instance HasField "y" Vec2 Float where
  getField :: Vec2 -> Float
  getField (Vec2 (V2 _ y)) = y

instance HasField "x" Vec3 Float where
  getField :: Vec3 -> Float
  getField (Vec3 (V3 x _ _)) = x

instance HasField "y" Vec3 Float where
  getField :: Vec3 -> Float
  getField (Vec3 (V3 _ y _)) = y

instance HasField "z" Vec3 Float where
  getField :: Vec3 -> Float
  getField (Vec3 (V3 _ _ z)) = z

extend :: Float -> Vec2 -> Vec3
extend z vec2 = vec3 (vec2.x, vec2.y, z)

trunc :: Vec3 -> Vec2
trunc vec3 = vec2 (vec3.x, vec3.y)

newtype Dir3 = Dir3 {inner :: V3 Float} deriving newtype (Show, Eq, Ord, Num)

instance HasField "vec" Dir3 Vec3 where
  getField :: Dir3 -> Vec3
  getField (Dir3 v3) = Vec3 v3

dir3 :: (Float, Float, Float) -> Dir3
dir3 (x, y, z) = Dir3 $ signorm $ V3 x y z