{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Relationships where

import Mischief.ECS.Components

data IsExclusiveRelationship = IsExclusiveRelationship deriving (Show, Component)
