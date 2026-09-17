{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Relationships.Tree where

import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Entities
import Mischief.ECS.Relationships.Graph
import Mischief.ECS.World

descendants :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
descendants entity = do
  next <- incoming @c entity
  next' <- mapM (descendants @c) next
  pure $ next ++ concat next'

ancestors' :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
ancestors' entity = do
  next <- outgoing' @c entity
  x <- mapM (ancestors' @c) next
  pure $ concat (next : x)

ancestors :: forall c m w. (Component c, MonadSystem w m, ListToOutgoing (RelOutgoing (IsExclusiveRel c) Entity)) => Entity -> m (RelOutgoing (IsExclusiveRel c) Entity)
ancestors e = listToOutgoing <$> ancestors' @c e

root :: forall c m w. (Component c, MonadSystem w m) => Entity -> m (Maybe Entity)
root entity = do
  out <- outgoing' @c entity
  case out of
    [] -> pure $ Just entity
    [p] -> root @c p
    _ -> undefined

leaves :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
leaves entity = do
  ing <- incoming @c entity
  case ing of
    [] -> return [entity]
    l -> do
      l' <- mapM (leaves @c) l
      pure $ concat l'
