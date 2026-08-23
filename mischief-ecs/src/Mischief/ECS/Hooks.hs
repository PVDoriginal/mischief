{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.Hooks where

-- ( relComplementary,
--   relCleanup,
--   relCleanupRemove,
--   relCleanupDespawn,
-- )

import Control.Monad
import Data.Data
import Data.Foldable
import Data.Kind
import Data.Maybe
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.HooksDef
import Mischief.ECS.Entities
import Mischief.ECS.EventDef
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.Utils
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Utils

instance EraseIntoStorage (HookContext -> System ()) (Hooks a) where
  erase :: (HookContext -> System ()) -> Hooks a
  erase x = Hooks [ErasedHook x]

instance EraseIntoStorage (Hooks a) (Hooks a) where
  erase = id

hook :: forall c a. (Collectable c (Hooks a)) => c -> Hooks a
hook = collect

hookRel :: forall c a. (Collectable c (HooksRel a)) => c -> HooksRel a
hookRel = collect

instance EraseIntoStorage (HookContextRel -> System ()) (HooksRel a) where
  erase :: (HookContextRel -> System ()) -> HooksRel a
  erase x = HooksRel [ErasedHookRel x]

instance EraseIntoStorage (HooksRel a) (HooksRel a) where
  erase = id
