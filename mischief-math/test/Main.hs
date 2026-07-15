module Main where

import Control.Monad.IO.Class
import Data.Default
import Mischief.ECS
import MischiefMath
import MischiefMath.Mat qualified as Mat
import MischiefMath.Quat qualified as Quat
import MischiefMath.Transform (GlobalTransform (..), Transform)
import MischiefMath.Transform qualified as Transform
import MischiefMath.Transform.Plugin (transformPlugin)
import MischiefMath.Vec (vec2, vec3)

quat = Quat.rotateX 1.7 Quat.identity

main :: IO ()
main = do
  app <- newApp [transformPlugin, testPropagation]
  runApp app

data Parent = Parent deriving (Component, Queryable)

data Child = Child deriving (Component, Queryable)

testPropagation :: Plugin ()
testPropagation = do
  addSystems Startup setup
  addSystems Update showTransforms

setup :: System ()
setup = do
  let parent = Transform.setRotation (Quat.fromRotationZ 0.5) $ Transform.setTranslation (vec3 (0, 1, 0)) $ def @Transform
  p <- spawn (Parent, parent)

  let child = Transform.setRotation (Quat.fromRotationZ $ -0.2) $ Transform.setTranslation (vec3 (0, 2, 0)) $ def @Transform
  _ <- spawn (Child, (child, R (ChildOf, p)))
  return ()

showTransforms :: System ()
showTransforms = do
  Just p <- single' @GlobalTransform $ with @Parent
  Just c <- single' @GlobalTransform $ with @Child

  let rot1 = Quat.toEuler p.value.transform.rotation
  let rot2 = Quat.toEuler c.value.transform.rotation
  liftIO $ print p
  liftIO $ print c
  liftIO $ print rot1
  liftIO $ print rot2
  return ()
