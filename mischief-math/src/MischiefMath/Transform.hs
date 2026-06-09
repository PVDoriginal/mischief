{-# OPTIONS_GHC -Wno-missing-methods #-}

module MischiefMath.Transform where

import Data.Default (Default)
import GHC.Generics (Generic)
import MischiefECS (Component, Queryable, require)
import MischiefECS.Components (Component (required))
import MischiefMath.Quat (Quat)
import MischiefMath.Quat qualified as Quat
import MischiefMath.Vec
import MischiefMath.Vec.Orphans.Default ()

newtype GlobalTransform = GlobalTransform {transform :: Transform}
  deriving anyclass (Component, Queryable, Default)
  deriving stock (Generic, Show)
  deriving newtype (Num)

data Transform = Transform {translation :: Vec3, rotation :: Quat, scale :: Vec3} deriving (Generic, Show, Default, Queryable)

instance Num Transform where
  (*) :: Transform -> Transform -> Transform
  (*) t1 t2 =
    Transform
      { translation = t1.translation + t2.translation,
        rotation = t1.rotation * t2.rotation,
        scale = t1.scale * t2.scale
      }

instance Component Transform where
  required = require @GlobalTransform

setRotation :: Quat -> Transform -> Transform
setRotation rotation Transform {translation, scale} =
  Transform {translation, rotation, scale}

setRotationAxis :: Dir3 -> Float -> Transform -> Transform
setRotationAxis dir angle Transform {translation, scale} =
  Transform {translation, rotation = Quat.fromAxisAngle dir angle, scale}

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
  Transform {translation, rotation = Quat.rotateAxis dir angle rotation, scale}

rotateX :: Float -> Transform -> Transform
rotateX angle Transform {translation, rotation, scale} =
  Transform {translation, rotation = Quat.rotateX angle rotation, scale}

rotateY :: Float -> Transform -> Transform
rotateY angle Transform {translation, rotation, scale} =
  Transform {translation, rotation = Quat.rotateY angle rotation, scale}

rotateZ :: Float -> Transform -> Transform
rotateZ angle Transform {translation, rotation, scale} =
  Transform {translation, rotation = Quat.rotateZ angle rotation, scale}
