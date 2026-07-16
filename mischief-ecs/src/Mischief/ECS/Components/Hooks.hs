module Mischief.ECS.Components.Hooks where

import Control.Monad
import Data.Foldable
import Mischief.ECS.Collectable
import Mischief.ECS.Events
import Mischief.ECS.World
import Mischief.ECS.World.Spawn

data ErasedHook c where
  ErasedHook :: forall e c. (Event (e c)) => (e c -> System ()) -> ErasedHook c

newtype Hooks c = Hooks [ErasedHook c] deriving newtype (Semigroup)

instance (Event (e c)) => EraseIntoStorage (e c -> System ()) (Hooks c) where
  erase :: (e c -> System ()) -> Hooks c
  erase x = Hooks [ErasedHook x]

registerHooks :: Hooks c -> System ()
registerHooks (Hooks h) = for_ h registerHook

registerHook :: ErasedHook c -> System ()
registerHook (ErasedHook (h :: e c -> System ())) =
  void $ spawn $ Observer h
