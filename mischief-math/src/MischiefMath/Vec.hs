module MischiefMath.Vec where

import Data.Default (Default (def))
import Linear (V2 (V2), V3 (V3), signorm)

newtype Vec2 = Vec2 (V2 Float) deriving newtype (Show, Eq, Ord, Num)

newtype Vec3 = Vec3 (V3 Float) deriving newtype (Show, Eq, Ord, Num)

vec2 :: (Float, Float) -> Vec2
vec2 (x, y) = Vec2 $ V2 x y

vec3 :: (Float, Float, Float) -> Vec3
vec3 (x, y, z) = Vec3 $ V3 x y z

instance Default Vec2 where
  def = vec2 (0, 0)

instance Default Vec3 where
  def = vec3 (0, 0, 0)

newtype Dir3 = Dir3 (V3 Float) deriving newtype (Show, Eq, Ord, Num)

dir3 :: (Float, Float, Float) -> Dir3
dir3 (x, y, z) = Dir3 $ signorm $ V3 x y z