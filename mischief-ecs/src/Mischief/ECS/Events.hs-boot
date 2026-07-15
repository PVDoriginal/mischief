module Mischief.ECS.Events where

import Data.Data

data ErasedEvent where
  ErasedEvent :: (Typeable e) => e -> ErasedEvent

class (Typeable e) => Event e where
  eraseEvent :: e -> ErasedEvent
  eraseEvent = ErasedEvent