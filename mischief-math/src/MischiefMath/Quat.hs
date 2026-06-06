module MischiefMath.Quat where

import Data.Default (Default (def))
import Linear (Quaternion (Quaternion), V3 (V3), axisAngle)
import MischiefMath.Vec (Dir3 (Dir3))

newtype Quat = Quat (Quaternion Float) deriving newtype (Show, Eq, Ord, Num)

instance Default Quat where
  def = Quat $ Quaternion 1 $ V3 0 0 0

fromAxisAngle :: Dir3 -> Float -> Quat
fromAxisAngle (Dir3 dir) angle = Quat $ axisAngle dir angle