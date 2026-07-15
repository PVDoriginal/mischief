{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module MischiefMath.Transform.Plugin where

import Control.Monad.IO.Class
import Mischief.ECS
import MischiefMath.Transform (GlobalTransform (GlobalTransform), Transform)

transformPlugin :: Plugin ()
transformPlugin = do
  addSystems First initGlobals
  addSystems First $ propagateTransform `after` initGlobals

initGlobals :: System ()
initGlobals = do
  iter' @(Entity, Transform) (added @Transform) $ \(entity, transform) -> do
    insert (GlobalTransform transform.value) entity

propagateTransform :: System ()
propagateTransform = do
  iter' @(Entity, Transform, R ChildOf) (changed @Transform) $ \(entity, transform, parents) -> do
    let [parent] = parents.targets
    Just parent <- get @GlobalTransform parent

    let newGlobal = parent.value * GlobalTransform transform.value
    insert newGlobal entity

    propagateRecursive entity newGlobal

propagateRecursive :: Entity -> GlobalTransform -> System ()
propagateRecursive parent parentGlobal = do
  iter' @(Entity, Transform) (withR @ChildOf parent) $ \(entity, transform) -> do
    let newGlobal = parentGlobal * GlobalTransform transform.value
    insert newGlobal entity

    propagateRecursive entity newGlobal