module MischiefECS.Utils where

import Data.Foldable
import MischiefECS.World.Internal
import MischiefECS.World.Par

parIter :: (MonadSystem w m, Foldable t) => t a -> (a -> ParSystem b) -> m ()
parIter x s = parIterList x $ \chunk -> for_ chunk s
