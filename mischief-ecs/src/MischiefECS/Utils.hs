module MischiefECS.Utils where

import Data.Foldable
import MischiefECS.World.Internal
import MischiefECS.World.Par

iter :: (MonadSystem w m, Foldable t) => m (t a) -> (a -> m b) -> m ()
iter x s = do
  x' <- x
  for_ x' s

parIter :: (MonadSystem w m, Foldable t) => m (t a) -> (a -> ParSystem b) -> m ()
parIter x s = do
  x' <- x
  parIterList x' $ \chunk -> for_ chunk s