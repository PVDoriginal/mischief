module Mischief.ECS.Components.Hooks where

import Control.Monad
import Data.Data
import Data.Foldable
import Mischief.ECS.Collectable
import Mischief.ECS.Components.HooksDef
import Mischief.ECS.EventDef
import Mischief.ECS.Events
import Mischief.ECS.World
import Mischief.ECS.World.Spawn (spawn)

instance (Event (e c)) => EraseIntoStorage (e c -> System ()) (Hooks c) where
  erase :: (e c -> System ()) -> Hooks c
  erase x = Hooks [ErasedHook x]

instance EraseIntoStorage (Hooks c) (Hooks c) where
  erase = id

registerHooks :: Hooks c -> System ()
registerHooks (Hooks h) = for_ h registerHook

registerHook :: ErasedHook c -> System ()
registerHook (ErasedHook (h :: e c -> m ())) =
  case eqT @m @System of
    Just Refl -> void $ spawn $ Observer h
    Nothing -> undefined
