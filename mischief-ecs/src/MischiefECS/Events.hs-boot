module MischiefECS.Events where

import Data.Data

data ErasedEvent where
  ErasedEvent :: (Typeable e) => e -> ErasedEvent
