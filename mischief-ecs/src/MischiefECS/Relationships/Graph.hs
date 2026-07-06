{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Relationships.Graph where

import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query

outgoing :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
outgoing entity = do
  next <- get @(Rel c) entity
  return $ case next of
    Nothing -> []
    Just next -> map target next

ingoing :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
ingoing entity = query' @Entity $ withRel @c entity
