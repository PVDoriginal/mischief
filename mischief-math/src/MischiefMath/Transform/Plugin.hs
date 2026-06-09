module MischiefMath.Transform.Plugin where

import MischiefECS
import MischiefMath.Transform (GlobalTransform (GlobalTransform), Transform)

transformPlugin :: Plugin ()
transformPlugin = do
  addSystems First propagateTransform

propagateTransform :: System ()
propagateTransform = do
  iter' @(Entity, Transform, GlobalTransform) (changed @Transform) $ \(entity, transform, global) -> do
    let newGlobal = GlobalTransform transform.value
    set global newGlobal

-- propagateRecursive entity newGlobal

-- propagateRecursive parent parentGlobal = do
