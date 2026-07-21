module Mischief.ECS.EventDef where

import Data.Data

-- | @Event@ typeclass.
class (Typeable e) => Event e where
  eraseEvent :: e -> ErasedEvent
  eraseEvent = ErasedEvent

data ErasedEvent where
  ErasedEvent :: (Typeable e) => e -> ErasedEvent