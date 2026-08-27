module Mischief.ECS.Components.HooksDef where

import Data.Data
import Mischief.ECS.EntityDef
import Mischief.ECS.EventDef

data Hook a where
  Hook :: (Typeable m) => (HookContext -> m ()) -> Hook a

data HookRel a where
  HookRel :: (Typeable m) => (HookContextRel -> m ()) -> HookRel a

newtype HookContext = HookContext {entity :: Entity}

data HookContextRel = HookContextRel {entity :: Entity, target :: Entity}
