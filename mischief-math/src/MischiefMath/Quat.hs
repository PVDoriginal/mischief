module MischiefMath.Quat where

import Data.Default (Default (def))
import Linear (Quaternion (Quaternion), V3 (V3), axisAngle, fromQuaternion)
import MischiefMath.Mat (Euler (Euler, x, y, z), Mat3 (Mat3))
import MischiefMath.Mat qualified as Mat
import MischiefMath.Vec (Dir3 (inner), Vec3, vec3)

newtype Quat = Quat (Quaternion Float) deriving newtype (Show, Eq, Ord, Num)

instance Default Quat where
  def = identity

identity :: Quat
identity = Quat $ Quaternion 1 $ V3 0 0 0

fromAxisAngle :: Dir3 -> Float -> Quat
fromAxisAngle dir angle = Quat $ axisAngle dir.inner angle

fromXYZW :: (Float, Float, Float, Float) -> Quat
fromXYZW (x, y, z, w) = Quat $ Quaternion w (V3 x y z)

fromRotationX :: Float -> Quat
fromRotationX angle =
  let (s, c) = (sin angle, cos angle)
   in fromXYZW (s, 0, 0, c)

fromRotationY :: Float -> Quat
fromRotationY angle =
  let (s, c) = (sin angle, cos angle)
   in fromXYZW (0, s, 0, c)

fromRotationZ :: Float -> Quat
fromRotationZ angle =
  let (s, c) = (sin angle, cos angle)
   in fromXYZW (0, 0, s, c)

fromAngles :: Vec3 -> Quat
fromAngles vec3 = rotateX vec3.x $ rotateY vec3.y $ rotateZ vec3.z identity

fromEuler :: Euler -> Quat
fromEuler Euler {x, y, z} = fromAngles $ vec3 (x, y, z)

rotateAxis :: Dir3 -> Float -> Quat -> Quat
rotateAxis dir angle quat = quat * fromAxisAngle dir angle

rotateX :: Float -> Quat -> Quat
rotateX angle quat = quat * fromRotationX angle

rotateY :: Float -> Quat -> Quat
rotateY angle quat = quat * fromRotationY angle

rotateZ :: Float -> Quat -> Quat
rotateZ angle quat = quat * fromRotationZ angle

toMat3 :: Quat -> Mat3
toMat3 (Quat quat) = Mat3 $ fromQuaternion quat

toEuler :: Quat -> Euler
toEuler = Mat.toEuler . toMat3
