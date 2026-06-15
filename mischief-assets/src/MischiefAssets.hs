module MischiefAssets where

import Data.ByteString (ByteString)
import GHC.IO.Handle

class Asset a where
  load :: ByteString -> a
