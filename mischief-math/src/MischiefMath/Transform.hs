module MischiefMath.Transform where

import Data.Default (Default)
import GHC.Generics (Generic)
import MischiefMath.Quat (Quat)
import MischiefMath.Quat qualified as Quat
import MischiefMath.Vec

data Transform = Transform {translation :: Vec3, rotation :: Quat, scale :: Vec3} deriving (Generic, Show, Default)

setRotation :: Quat -> Transform -> Transform
setRotation rotation Transform {translation, scale} =
  Transform {translation, rotation, scale}

setTranslation :: Vec3 -> Transform -> Transform
setTranslation translation Transform {rotation, scale} =
  Transform {translation, rotation, scale}

setScale :: Vec3 -> Transform -> Transform
setScale scale Transform {rotation, translation} =
  Transform {translation, rotation, scale}

translate :: Vec3 -> Transform -> Transform
translate trans Transform {translation, rotation, scale} =
  Transform {translation = translation + trans, rotation, scale}

scale :: Vec3 -> Transform -> Transform
scale sc Transform {translation, rotation, scale} =
  Transform {translation, rotation, scale = scale * sc}

rotate :: Quat -> Transform -> Transform
rotate rot Transform {translation, rotation, scale} =
  Transform {translation, rotation = rotation * rot, scale}

rotateAxis :: Dir3 -> Float -> Transform -> Transform
rotateAxis dir angle Transform {translation, rotation, scale} =
  Transform {translation, rotation = rotation * Quat.fromAxisAngle dir angle, scale}
