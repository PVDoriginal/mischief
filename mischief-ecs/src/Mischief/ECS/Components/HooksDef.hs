module Mischief.ECS.Components.HooksDef where

import Data.Data
import Mischief.ECS.EventDef

data ErasedHook c where
  ErasedHook :: forall e c m. (Event (e c), Typeable m) => (e c -> m ()) -> ErasedHook c

newtype Hooks c = Hooks [ErasedHook c] deriving newtype (Semigroup)
