module Mischief.ECS.Components.Common where

import Mischief.ECS.Components

-- | Special component inserted automatically on most entities.
--
-- Can be inserted manually to give each entity a custom name.
newtype Name = Name String
  deriving anyclass (Component)
  deriving newtype (Eq)

instance Show Name where
  show :: Name -> String
  show (Name name) = show name
