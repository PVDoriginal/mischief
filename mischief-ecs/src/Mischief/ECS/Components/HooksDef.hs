module Mischief.ECS.Components.HooksDef where

import Data.Data
import Mischief.ECS.EntityDef
import Mischief.ECS.EventDef

data ErasedHook where
  ErasedHook :: forall m. (Typeable m) => (HookContext -> m ()) -> ErasedHook

newtype Hooks a = Hooks {inner :: [ErasedHook]} deriving newtype (Semigroup)

newtype HookContext = HookContext {entity :: Entity}

data ErasedHookRel where
  ErasedHookRel :: forall m. (Typeable m) => (HookContextRel -> m ()) -> ErasedHookRel

newtype HooksRel a = HooksRel {inner :: [ErasedHookRel]} deriving newtype (Semigroup)

data HookContextRel = HookContextRel {entity :: Entity, target :: Entity}
