{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Relationships where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import MischiefECS.App
import MischiefECS.Components
import MischiefECS.Events
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query
import MischiefECS.World.Remove

data Exclusivity = Inclusive | Exclusive deriving (Show, Eq)

newtype RelationshipSettings = RelationshipSettings {exclusivity :: Exclusivity}

instance Default RelationshipSettings where
  def = RelationshipSettings {exclusivity = Inclusive}

addRelationship :: forall c. (Component c) => Plugin ()
addRelationship = addRelationshipWithSettings @c $ def @RelationshipSettings

addRelationshipWithSettings :: forall c. (Component c) => RelationshipSettings -> Plugin ()
addRelationshipWithSettings settings = do
  when (settings.exclusivity == Exclusive) $ do
    addObserverOrdered (handleExclusivity @c) $ -1000

handleExclusivity :: forall c. (Component c) => OnInsertR c -> System ()
handleExclusivity event = do
  Just relationships <- get @(R c) event.entity

  when (event.target `elem` relationships.targets) $
    for_ relationships.collection $ \r ->
      when (r.target /= event.target) $ delete r
