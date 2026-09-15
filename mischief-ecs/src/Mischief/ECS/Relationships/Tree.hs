{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Relationships.Tree where

import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Entities
import Mischief.ECS.Relationships.Graph
import Mischief.ECS.World

descendants :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
descendants entity = do
  next <- ingoing @c entity
  next' <- mapM (descendants @c) next
  pure $ next ++ concat next'

anestors :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
anestors entity = do
  next <- outgoing @c entity
  x <- mapM (anestors @c) next
  pure $ concat (next : x)

root :: forall c m w. (Component c, MonadSystem w m) => Entity -> m (Maybe Entity)
root entity = do
  out <- outgoing @c entity
  case out of
    [] -> pure $ Just entity
    [p] -> root @c p
    _ -> undefined

leaves :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
leaves entity = do
  ing <- ingoing @c entity
  case ing of
    [] -> return [entity]
    l -> do
      l' <- mapM (leaves @c) l
      pure $ concat l'
