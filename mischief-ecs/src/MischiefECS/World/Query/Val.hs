{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module contains instances of 'Mappable' for the 'Val' query marker.
--
-- These instances are technically orphaned, but that's fine because the 'Val type
-- makes them unique and 'Mappable' isn't exported outside of this package.
module MischiefECS.World.Query.Val where

import MischiefECS.Entities
import MischiefECS.Mappable (Mappable)
import MischiefECS.Mappable qualified as Mappable
import MischiefECS.Tables
import MischiefECS.World.Query.Queryable

instance {-# OVERLAPPABLE #-} Mappable MapQueryVal Entity Entity where
  map = id

instance {-# OVERLAPPABLE #-} Mappable MapQueryVal Bool Bool where
  map = id

instance {-# OVERLAPPABLE #-} Mappable MapQueryVal (Maybe (Result a)) (Maybe a) where
  map = fmap (Mappable.map @MapQueryVal)

instance {-# OVERLAPPING #-} Mappable MapQueryVal (Result a) a where
  map (Result (a, _)) = a