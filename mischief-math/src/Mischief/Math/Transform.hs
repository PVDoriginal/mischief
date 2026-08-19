{-# OPTIONS_GHC -Wno-missing-methods #-}

module Mischief.Math.Transform where

import Data.Default (Default)
import GHC.Generics (Generic)
import Mischief.ECS.Prelude
import Mischief.Math.Quat (Quat)
import Mischief.Math.Quat qualified as Quat
import Mischief.Math.Vec
import Mischief.Math.Vec.Orphans.Default ()

newtype GlobalTransform = GlobalTransform {transform :: Transform}
  deriving anyclass (Component, Default)
  deriving stock (Generic, Show)
  deriving newtype (Num)

data Transform = Transform {translation :: Vec3, rotation :: Quat, scale :: Vec3} deriving (Generic, Show, Default, Component)

instance Num Transform where
  (*) :: Transform -> Transform -> Transform
  (*) t1 t2 =
    Transform
      { translation = t1.translation + t2.translation,
        rotation = t1.rotation * t2.rotation,
        scale = t1.scale * t2.scale
      }

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
