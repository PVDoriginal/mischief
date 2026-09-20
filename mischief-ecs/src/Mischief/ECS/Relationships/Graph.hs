{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.Relationships.Graph where

import Data.List
import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Entities
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable

type family RelOutgoing b where
  RelOutgoing True = Maybe
  RelOutgoing False = List

class ListToOutgoing b where
  listToOutgoing :: [Entity] -> b

instance ListToOutgoing (Maybe Entity) where
  listToOutgoing [x] = Just x
  listToOutgoing _ = Nothing

instance ListToOutgoing [Entity] where
  listToOutgoing = id

outgoing' :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
outgoing' entity = do
  next <- single $ mkGet entity (R' @c Any)
  return $ case next of
    Nothing -> []
    Just next -> map (\x -> x.target) next

outgoing :: forall c m w. (Component c, MonadSystem w m, ListToOutgoing (RelOutgoing (IsExclusiveRel c) Entity)) => Entity -> m (RelOutgoing (IsExclusiveRel c) Entity)
outgoing entity = listToOutgoing <$> outgoing' @c entity

incoming :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
incoming entity = query $ mkQuery' E (With (R @c entity))
