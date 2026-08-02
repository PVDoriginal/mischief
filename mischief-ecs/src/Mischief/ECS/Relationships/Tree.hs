{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Relationships.Tree where

import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Entities
import Mischief.ECS.Relationships.Graph
import Mischief.ECS.World

-- descendants :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
-- descendants entity = do
--   next <- ingoing @c entity
--   next' <- mapM (descendants @c) next
--   return $ next ++ concat next'

-- anestors :: forall c m w. (Component c, MonadSystem w m) => Entity -> m [Entity]
-- anestors entity = do
--   next <- outgoing @c entity
--   x <- mapM (anestors @c) next
--   return $ concat (next : x)

-- root :: forall c m w. (Component c, MonadSystem w m) => Entity -> m Entity
-- root entity = do
--   out <- outgoing @c entity
--   case out of
--     [] -> return entity
--     [p] -> root @c p
--     _ -> undefined

-- leaves :: forall c m w. (Component c, BundleTypes c, MonadSystem w m) => Entity -> m [Entity]
-- leaves entity = do
--   ing <- ingoing @c entity
--   case ing of
--     [] -> return [entity]
--     l -> do
--       l' <- mapM (leaves @c) l
--       return $ concat l'
