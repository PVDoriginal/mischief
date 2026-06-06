module MischiefMath.Transform where

import Data.Default (Default)
import GHC.Generics (Generic)
import MischiefMath.Quat (Quat)
import MischiefMath.Quat qualified as Quat
import MischiefMath.Vec

data Transform = Transform {translation :: Vec3, rotation :: Quat, scale :: Vec3} deriving (Generic, Show, Default)

withRotation :: Transform -> Quat -> Transform
withRotation Transform {translation, scale} rotation =
  Transform {translation, rotation, scale}

withTranslation :: Transform -> Vec3 -> Transform
withTranslation Transform {rotation, scale} translation =
  Transform {translation, rotation, scale}

withScale :: Transform -> Vec3 -> Transform
withScale Transform {rotation, translation} scale =
  Transform {translation, rotation, scale}

translate :: Transform -> Vec3 -> Transform
translate Transform {translation, rotation, scale} trans =
  Transform {translation = translation + trans, rotation, scale}

scale :: Transform -> Vec3 -> Transform
scale Transform {translation, rotation, scale} sc =
  Transform {translation, rotation, scale = scale * sc}

rotate :: Transform -> Quat -> Transform
rotate Transform {translation, rotation, scale} rot =
  Transform {translation, rotation = rotation * rot, scale}

rotateAxis :: Transform -> Dir3 -> Float -> Transform
rotateAxis Transform {translation, rotation, scale} dir angle =
  Transform {translation, rotation = rotation * Quat.fromAxisAngle dir angle, scale}
