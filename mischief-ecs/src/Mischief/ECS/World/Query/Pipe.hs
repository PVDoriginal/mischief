{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.World.Query.Pipe
  ( -- * Impure
    qtraverse,
    qtap,

    -- * Mapping
    qmap,
    qmapM,
    qmapMaybe,
    qmapMaybeM,

    -- * Filtering
    qcheck,
    qfilter,
    qfilterM,

    -- * Insertion
    qinsert,
    qinsertNew,
    qinsertIfNeq,
    qmodify,
    qmodifyNew,
    qmodifyIfNeq,

    -- * Expansion
    qextend,
    qentity,

    -- * Traversal
    qrelateOne,
    qrelateMany,
    qjoin,

    -- * Folds
    qfoldr,
    qcollect,

    -- * Monad
    qpure,
    qrefocus,
    qres,
    qget,
    qgetAll,

    -- * Logging
    qinfo,
    qwarn,
    qerr,

    -- * Other
    qdespawn,
  )
where

import Control.Monad.IO.Class
import Data.Function
import Data.Maybe
import Data.Text (Text)
import GHC.Stack
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.Log
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Spawn

{-# RULES
"qmap/qmap" forall f g xs. qmap f (qmap g xs) = qmap (f . g) xs
  #-}

qfoldr :: (MonadSystem w m) => (From a -> b -> b) -> b -> Entity -> Query m a -> Query m b
qfoldr f b e a = FoldQuery a e f b

qcollect :: (MonadSystem w m) => Entity -> Query m a -> Query m [From a]
qcollect = qfoldr (:) []

-- | Maps @Query m a@ to @Query m b@. Same as 'fmap'.
--
-- __Example__
--
-- @
-- [q|Name, Position|]
--   & qmap (\\(name, _) -> name)
--   & query
-- @
qmap :: (MonadSystem w m) => (a -> b) -> Query m a -> Query m b
qmap = fmap
{-# INLINE [1] qmap #-}

-- | 'qmap' with side effects. Same as 'qtraverse'.
qmapM :: (MonadSystem w m) => (Entity -> a -> m b) -> Query m a -> Query m b
qmapM = qtraverse

-- | 'qmapMaybe' with side effects.
qmapMaybeM :: (MonadSystem w m) => (Entity -> a -> m (Maybe b)) -> Query m a -> Query m b
qmapMaybeM f b = MapQuery (FilterQuery (MapQuery b f) (\_ x -> pure (isJust x))) (\_ x -> pure (fromMaybe undefined x))

-- | Possibly maps each element from @Query m a@ to an element from @Query m b@. If Nothing, will filter this Entity out of the Query.
--
-- __Example__
--
-- @
-- f :: Name -> Maybe Name
-- @
--
-- @
-- [q|Name|]
--   & qmapMaybe f
--   & query
-- @
qmapMaybe :: (MonadSystem w m) => (a -> Maybe b) -> Query m a -> Query m b
qmapMaybe f x = qmap (fromMaybe undefined) $ qfilter isJust (qmap f x)

-- | Map @Query m a@ to @Query m b@ with side effects.
--
-- __Example__
--
-- @
-- [q|Name, Position|]
--   & qtraverse (\\e (name, pos) -> do
--     [i|The name of entity #{e} is #{name}|]
--     pure pos
--   )
--   & query
-- @
qtraverse :: (Entity -> a -> m b) -> Query m a -> Query m b
qtraverse f x = MapQuery x f

-- | Apply a side effect over a query.
--
-- __Example__
--
-- @
-- [q|Name, Position|]
--   & qtraverse (\\e (name, pos) -> do
--     [i|The name of entity #{e} is #{name}|]
--   )
--   & query
-- @
qtap :: (Entity -> a -> m b) -> Query m a -> Query m a
qtap = flip DoQuery

-- | Insert the values obtained from a mapping function.
--
-- __Example__
--
-- @
-- q[|Position, Velocity|]
--   & qinsert (\\(Position p, Velocity v) -> Position (p + v))
--   & query
-- @
qinsert :: (Bundle b) => (a -> b) -> Query System a -> Query System a
qinsert f = qtap (\e a -> insert (f a) e)

-- | Same as 'qinsert' but only inserts components that aren't already on the entity.
qinsertNew :: (Bundle b) => (a -> b) -> Query System a -> Query System a
qinsertNew f = qtap (\e a -> insertNew (f a) e)

-- | Same as 'qinsert' but only inserts components that are new or different than the ones already on the entity. Avoids triggering false change detection.
qinsertIfNeq :: (BundleEq b) => (a -> b) -> Query System a -> Query System a
qinsertIfNeq f = qtap (\e a -> insertIfNeq (f a) e)

-- | Same as 'qinsert' but will also propagate the mapped elements. Equivalent to @qinsert id . qmap f@.
qmodify :: (Bundle b) => (a -> b) -> Query System a -> Query System b
qmodify f = qinsert id . qmap f

-- | Same as 'qinsertNew' but will also propagate the mapped elements. Equivalent to @qinsertNew id . qmap f@.
qmodifyNew :: (Bundle b) => (a -> b) -> Query System a -> Query System b
qmodifyNew f = qinsertNew id . qmap f

-- | Same as 'qinsertIfNeq' but will also propagate the mapped elements. Equivalent to @qinsertIfNeq id . qmap f@.
qmodifyIfNeq :: (BundleEq b) => (a -> b) -> Query System a -> Query System b
qmodifyIfNeq f = qinsertIfNeq id . qmap f

{-# RULES
"qfilter/qfilter" forall f g xs. qfilter f (qfilter g xs) = qfilter (\x -> f x && g x) xs
  #-}

-- | Filter the entities of a query.
--
-- __Example__
--
-- @
-- [q|Name|]
--   & qfilter (== Name "Florian")
--   & query
-- @
qfilter :: (MonadSystem w m) => (a -> Bool) -> Query m a -> Query m a
qfilter f x = FilterQuery x (\_ x -> pure (f x))
{-# INLINE [1] qfilter #-}

-- | Filter the entities of a query with possible side effects.
--
-- __Example__
--
-- @
-- [q|Position|]
--   & qfilterM (\\e _ -> 'changed' \@Position e)
--   & query
-- @
qfilterM :: (Entity -> a -> m Bool) -> Query m a -> Query m a
qfilterM f x = FilterQuery x f

check :: (MonadSystem w m) => QueryFilter EntityFilter -> Entity -> m Bool
check NoFilter _ = pure True
check (With x) e = do
  world <- unsafeGetWorld
  liftIO $ filterEntity (With x) world e
check (Without x) e = do
  world <- unsafeGetWorld
  liftIO $ filterEntity (Without x) world e
check (Added x) e = added x e
check (Changed x) e = changed x e
check (And a b) e = (&&) <$> check a e <*> check b e
check (Or a b) e = (||) <$> check a e <*> check b e
check (Not a) e = not <$> check a e

{-# RULES
"qcheck/qcheck" forall f g xs. qcheck f (qcheck g xs) = qcheck (f `And` g) xs
  #-}

-- | Filter the entities of a query with a dedicated query filter.
--
-- __Example__
--
-- @
-- [q|Position|]
--   & qcheck [qf|Changed Position, !Added Position|]
--   & query
-- @
qcheck :: (MonadSystem w m) => QueryFilter EntityFilter -> Query m a -> Query m a
qcheck f = qfilterM (\e _ -> check f e)
{-# INLINE [1] qcheck #-}

-- | Extends the query with new elements.
--
-- __Example__
--
-- @
-- [q|Name|]
--   & qextend [qd|Position|] (,)
--   & query
-- @
qextend :: (MonadSystem w m, Queryable qd out) => qd -> (a -> out -> c) -> Query m a -> Query m c
qextend qd f a = do
  (e, a) <- qentity a
  qget e qd
    & qcollect e
    & qmap (\case [From _ a'] -> f a a'; _ -> undefined)

-- | Grab a Query Data from an entity resulting from a traversal and map it into the current data wrapped in a @From@:
--
-- __Example__
--
-- @
-- [q|Name|]
--   & qrelateOne (Tree.'Mischief.ECS.Relationships.Tree.root' \@Like) \[qd|Name|] (,)
--   & query_
-- @
qrelateOne :: (MonadSystem w m, Queryable qd out) => (Entity -> m (Maybe Entity)) -> qd -> (a -> From out -> c) -> Query m a -> Query m c
qrelateOne f qd f' a = do
  (a, e) <- a & qtraverse (\e a -> (a,) <$> f e)
  case e of
    Nothing -> EmptyQuery
    Just e -> do
      out <- qget e qd
      pure $ f' a (From e out)

-- | Same as 'qrelateOne' but takes a traversal that returns a list of entities.
qrelateMany :: (MonadSystem w m, Queryable (E, qd) (Entity, out)) => (Entity -> m [Entity]) -> qd -> (a -> [From out] -> c) -> Query m a -> Query m c
qrelateMany f qd f' a = do
  ((ae, a), e) <- qentity a & qtraverse (\e a -> (a,) <$> f e)
  case e of
    [] -> EmptyQuery
    e -> do
      as <-
        qgetAll e (E, qd)
          & qmap (uncurry From)
          & qcollect ae
          & qmap (map (.comp))
      pure $ f' a as

qres :: forall c m w. (MonadSystem w m, Component c) => Query m (Res c)
qres = qget (Entity (# 0##, 0## #)) (Res @c)

qget :: (MonadSystem w m, Queryable qd out) => Entity -> qd -> Query m out
qget entity qd = BuildQuery qd NoFilter (Just [entity])

qgetAll :: (MonadSystem w m, Queryable qd out) => [Entity] -> qd -> Query m out
qgetAll entity qd = BuildQuery qd NoFilter (Just entity)

qrefocus :: Entity -> Query m a -> Query m a
qrefocus = flip RefocusQuery

qpure :: (MonadSystem w m) => Entity -> Query m ()
qpure e = qrefocus e $ pure ()

--  | Pairs the query's elements with their entity.
qentity :: (MonadSystem w m) => Query m a -> Query m (Entity, a)
qentity = PairEntityQuery

qjoin :: (MonadSystem w m) => (a -> Query m b) -> (a -> [From b] -> c) -> Query m a -> Query m c
qjoin f f' a = do
  (e, a) <- qentity a
  f a & qcollect e & qfilter (not . null) & qmap (f' a)

-- | Logs an INFO message.
qinfo :: (HasCallStack, MonadSystem w m) => (a -> Text) -> Query m a -> Query m a
qinfo f a = withFrozenCallStack $ qtap (\_ x -> info (f x)) a

-- | Logs a WARNING message.
qwarn :: (HasCallStack, MonadSystem w m) => (a -> Text) -> Query m a -> Query m a
qwarn f a = withFrozenCallStack $ qtap (\_ x -> warn (f x)) a

-- | Logs an ERROR message.
qerr :: (HasCallStack, MonadSystem w m) => (a -> Text) -> Query m a -> Query m a
qerr f a = withFrozenCallStack $ qtap (\_ x -> err (f x)) a

qdespawn :: Query System a -> Query System a
qdespawn = qtap (\e _ -> despawn e)
