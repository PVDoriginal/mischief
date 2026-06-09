{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module MischiefMath.Transform.Plugin where

import MischiefECS
import MischiefMath.Transform (GlobalTransform (GlobalTransform), Transform)

-- transformPlugin :: Plugin ()
-- transformPlugin = do
--   addSystems First propagateTransform

-- propagateTransform :: System ()
-- propagateTransform = do
--   iter' @(Entity, Transform, R ChildOf) (changed @Transform) $ \(entity, transform, parents) -> do
--     let [parent] = parents.targets
--     let newGlobal = GlobalTransform transform.value
--     set global newGlobal

-- propagateRecursive entity newGlobal

-- propagateRecursive parent parentGlobal = do
