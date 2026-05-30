module MischiefECS.Events.Internal where

import Data.Data

data ErasedEvent where
  ErasedEvent :: (Typeable e) => e -> ErasedEvent
