{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module contains instances of 'Mappable' for the 'MapQueryVal' marker.
--
-- These instances are technically orphaned, but that's fine because the
-- 'MapQueryVal' isn't exported from this package so other orphaned instances
-- can't be created outside of it.
module Mischief.ECS.World.Query.Val where

import Mischief.ECS.Entities
import Mischief.ECS.Mappable (Mappable)
import Mischief.ECS.Mappable qualified as Mappable
import Mischief.ECS.Tables
import Mischief.ECS.World.Query.Queryable