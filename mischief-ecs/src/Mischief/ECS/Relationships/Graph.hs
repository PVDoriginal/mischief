{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Relationships.Graph where

import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Query

outgoing :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
outgoing entity = do
  next <- get @(Rel c) entity
  return $ case next of
    Nothing -> []
    Just next -> map target next

ingoing :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
ingoing entity = query' @Entity $ withRel @c entity
