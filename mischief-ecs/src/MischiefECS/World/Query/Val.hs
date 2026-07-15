{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module contains instances of 'Mappable' for the 'MapQueryVal' marker.
--
-- These instances are technically orphaned, but that's fine because the
-- 'MapQueryVal' isn't exported from this package so other orphaned instances
-- can't be created outside of it.
module MischiefECS.World.Query.Val where

import MischiefECS.Entities
import MischiefECS.Mappable (Mappable)
import MischiefECS.Mappable qualified as Mappable
import MischiefECS.Tables
import MischiefECS.World.Query.Queryable