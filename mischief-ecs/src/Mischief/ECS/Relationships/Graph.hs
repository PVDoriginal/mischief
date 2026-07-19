{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.Relationships.Graph where

import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Entities
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable

outgoing :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
outgoing entity = do
  next <- get (R @c Any) entity
  return $ case next of
    Nothing -> []
    Just next -> map (\x -> x.target) next

ingoing :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
ingoing entity = query' E (WithR @c entity)
