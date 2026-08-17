module Mischief.ECS.Foreign where

import Control.Monad.IO.Class
import Foreign (Ptr, Storable)
import Foreign.Marshal.Alloc as A
import Mischief.ECS.World

alloca :: (Storable a) => (Ptr a -> System b) -> System b
alloca s = do
  x <- sysUnliftIO
  liftIO $ A.alloca $ x . s