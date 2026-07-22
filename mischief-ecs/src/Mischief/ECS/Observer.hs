module Mischief.ECS.Observer where

import Data.Data
import Data.Default
import GHC.Generics
import Mischief.ECS.Components
import Mischief.ECS.Components.Required
import Mischief.ECS.World

newtype Observer e = Observer (e -> System ())

instance (Typeable e) => Component (Observer e) where
  required = require @(EventProxy e, ObserverOrder)

newtype ObserverOrder = ObserverOrder Int
  deriving (Show)
  deriving anyclass (Component)
  deriving newtype (Eq, Ord, Default)

data EventProxy e = EventProxy deriving (Component, Generic, Default)
