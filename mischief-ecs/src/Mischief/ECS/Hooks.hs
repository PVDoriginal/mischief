module Mischief.ECS.Hooks (hook, hookRel) where

import Mischief.ECS.Components.HooksDef
import Mischief.ECS.World

hook :: (HookContext -> System ()) -> Hook a
hook = Hook

hookRel :: (HookContextRel -> System ()) -> HookRel a
hookRel = HookRel