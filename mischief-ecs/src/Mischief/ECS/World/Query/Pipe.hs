{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Mischief.ECS.World.Query.Pipe
  ( -- * Impure
    qtraverse,
    qtap,

    -- * Chaining
    qthen,

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
    qres,
    qentity,

    -- * Traversal
    qget,
    qgetM,
    qgetMany,
    qgetManyM,
    qrelate,
    qrelateMany,
    qcross,
    qcrossM,

    -- * Joins
    qjoin,
    qjoinOuter,

    -- * Logging
    qinfo,
    qwarn,
    qerr,
  )
where

import Control.Monad (filterM)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function
import Data.List (List)
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import GHC.Stack
import Language.Haskell.TH (Extension (AllowAmbiguousTypes))
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Entities
import Mischief.ECS.Log
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable

{-# RULES
"qmap/qmap" forall f g xs. qmap f (qmap g xs) = qmap (f . g) xs
  #-}

qthen :: (MonadSystem w m) => (a -> Query m b) -> Query m a -> Query m b
qthen = flip BindQuery

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

-- | Same as 'qtraverse'.
qmapM :: (MonadSystem w m) => (Entity -> a -> m b) -> Query m a -> Query m b
qmapM = qtraverse

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

-- mapFilterQuery
--   x
--   ( \_ a ->
--       pure $ f a
--   )

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
--   & qcheck [f|Changed Position, !Added Position|]
--   & query
-- @
qcheck :: (MonadSystem w m) => QueryFilter EntityFilter -> Query m a -> Query m a
qcheck f = qfilterM (\e _ -> check f e)
{-# INLINE [1] qcheck #-}

-- | Grab a specific entity from another query and map it into the current one. If the first function returns Nothing, will filter this entity
-- out of the query.
--
--  __Example__
--
-- @
-- data Sprite = Sprite {image :: Entity} deriving (Component)
-- @
--
-- @
-- [q|Sprite|]
--   qget (\\sprite -> sprite.image) (,) [q|Image|]
--   & query
-- @
qget :: (MonadSystem w m) => (a -> Maybe Entity) -> Query m b -> Query m a -> Join m a Maybe (From b)
qget f b a =
  Join
    a
    ( \_ x -> do
        let entity = f x
        case entity of
          Nothing -> pure Nothing
          Just entity -> do
            bs <- get entity b
            pure $ fmap (From entity) bs
    )

-- Like 'qget' but with side-effects in the function which provides the entity.
qgetM :: (MonadSystem w m) => (Entity -> a -> m (Maybe Entity)) -> Query m b -> Query m a -> Join m a Maybe (From b)
qgetM f b a =
  Join
    a
    ( \e x -> do
        entity <- f e x
        case entity of
          Nothing -> pure Nothing
          Just entity -> do
            bs <- get entity b
            pure $ fmap (From entity) bs
    )

-- | Grab specific entities from another query. If the first function returns an empty list, will filter this entity out of the query.
--
-- __Example__
--
-- @
-- data Images = Images {images :: [Entity]} deriving (Component)
-- @
--
-- @
-- [q|Images|]
--   & qgetMany (.images) (,) [q|Image|]
--   & query
-- @
qgetMany :: (MonadSystem w m) => (a -> [Entity]) -> Query m b -> Query m a -> Join m a List (From b)
qgetMany f b a =
  Join
    a
    ( \_ x -> do
        let entities = f x
        y <- catMaybes <$> mapM (\e -> fmap (e,) <$> get e b) entities
        pure $ map (uncurry From) y
    )

-- | Like 'qgetMany' but with side-effects in the function which provides the entities.
qgetManyM :: (MonadSystem w m) => (a -> m [Entity]) -> Query m b -> Query m a -> Join m a List (From b)
qgetManyM f b a =
  Join
    a
    ( \_ x -> do
        entities <- f x
        y <- catMaybes <$> mapM (\e -> fmap (e,) <$> get e b) entities
        pure $ map (uncurry From) y
    )

-- | Very similar to 'qappendOneM'. This is a helper function mainly intended to make relationship traversals easier.
--
-- __Example__
--
-- @
-- import Mischief.ECS.Relationships.Graph
-- @
--
-- @
-- [q|Name|]
--   & qrelateOne (Graph.outgoing \@ChildOf) (,) [q|Name|]
--   & qinfo (\\(child, parent) -> [i|#{child} is child of #{parent}|])
--   & query_
-- @
qrelate :: (MonadSystem w m) => (Entity -> m (Maybe Entity)) -> Query m b -> Query m a -> Join m a Maybe (From b)
qrelate f b a =
  Join
    a
    ( \e _ -> do
        entity <- f e
        case entity of
          Nothing -> pure Nothing
          Just entity -> do
            y <- get entity b
            pure $ fmap (From entity) y
    )

-- | Very similar to 'qappendManyM'. This is a helper function mainly intended to make relationship traversals easier.
--
-- __Example__
--
-- @
-- import Mischief.ECS.Relationships.Graph
-- @
--
-- @
-- [q|Name|]
--   & qrelateOne (Graph.ingoing \@ChildOf) (,) [q|Name|]
--   & qinfo (\\(parent, children) -> [i|#{parent} is parent of #{children}|])
--   & query_
-- @
qrelateMany :: (MonadSystem w m) => (Entity -> m [Entity]) -> Query m b -> Query m a -> Join m a List (From b)
qrelateMany f b a =
  Join
    a
    ( \e _ -> do
        entities <- f e
        y <- catMaybes <$> mapM (\e -> fmap (e,) <$> get e b) (toList entities)
        pure $ map (uncurry From) y
    )

-- qjoin :: (MonadSystem w m) => (a -> b -> Bool) -> (a -> From b -> c) -> Query m b -> Query m a -> Query m [c]
-- qjoin f f' a b =
--   mapFilterQuery
--     b
--     ( \_ x -> do
--         y <- query (qentity a)
--         let b = map (\(e, y) -> f' x (From e y)) $ filter (f x . snd) y
--         case b of
--           [] -> pure Nothing
--           b -> pure $ Just b
--     )

-- t :: Query m ()
-- t = do
--   let x :: [a] = undefined
--   for x $ \x -> undefined
--   undefined

-- | Grab the elements from another query that meet a certain condition in relation to this query. Map them together.
-- The entities for which the list is empty will be filtered out.
--
-- __Example__
--
-- @
-- [q|Entity, Name|]
--   & qjoin (\\(e1, n1) (e2, n2) -> n1 == n2 && e1 /= e2) (,) [q|Entity, Name|]
--   & qinfo (\\((e1, name), (e2, _)) -> [i|#{e1} and #{e2} are both named #{name}|])
--   & query_
-- @
qcross :: (MonadSystem w m) => (a -> b -> Bool) -> Query m b -> Query m a -> Join m a List (From b)
qcross f b a =
  Join
    a
    ( \_ a -> do
        y <- query (qentity b)
        let y' = filter (\(_, b) -> f a b) y
        pure $ map (uncurry From) y'
    )

-- | Same as 'qcross' but with side effects in the function which matches the elements of the two queries.
qcrossM :: (MonadSystem w m) => (Entity -> a -> Entity -> b -> m Bool) -> Query m b -> Query m a -> Join m a List (From b)
qcrossM f b a =
  Join
    a
    ( \e a -> do
        y <- query (qentity b)
        y' <- filterM (uncurry (f e a)) y
        pure $ map (uncurry From) y'
    )

-- | Extends the query with new elements, grabbed in O(1).
--
-- __Example__
--
-- @
-- [q|Name|]
--   & qextend (,) [q|Position|]
--   & query
-- @
qextend :: (MonadSystem w m) => Query m b -> Query m a -> Join m a Maybe b
qextend b a = Join a (\e _ -> get e b)

-- | Maps a resource into the query. If the resource doesn't exist, the query will stop.
--
-- Same as @qextend f [q|Res \@r|]@.
--
-- __Example__
--
-- @
-- [q|Name|]
--   & qres \@SomeResource (,)
--   & query
-- @
qres :: forall r m a w. (MonadSystem w m, Component r) => Query m a -> Join m a Maybe (Res r)
qres = qextend (mkQuery (Res @r))

-- | Pairs the query's elements with their entity. Same as @qextend (flip (,)) [q|Entity|]@.
qentity :: (MonadSystem w m) => Query m a -> Query m (Entity, a)
qentity a = qextend (mkQuery E) a & qjoin (flip (,))

-- mapFilterQuery
--   a
--   ( \_ a -> do
--       m <- tryMeta @r
--       case m of
--         Nothing -> pure Nothing
--         Just m -> do
--           r <- get m $ mkQuery (C @r)
--           case r of
--             Nothing -> pure Nothing
--             Just r -> pure $ Just $ f a r
--   )

-- qtry :: (MonadSystem w m) => ((a -> b -> c) -> Query m a -> Query m c) -> Query m a ->
-- qtry = undefined

-- | Logs an INFO message.
qinfo :: (HasCallStack, MonadSystem w m) => (a -> Text) -> Query m a -> Query m a
qinfo f a = withFrozenCallStack $ qtap (\_ x -> info (f x)) a

-- | Logs a WARNING message.
qwarn :: (HasCallStack, MonadSystem w m) => (a -> Text) -> Query m a -> Query m a
qwarn f a = withFrozenCallStack $ qtap (\_ x -> warn (f x)) a

-- | Logs an ERROR message.
qerr :: (HasCallStack, MonadSystem w m) => (a -> Text) -> Query m a -> Query m a
qerr f a = withFrozenCallStack $ qtap (\_ x -> err (f x)) a

data Join m a t b = Join (Query m a) (Entity -> a -> m (t b))

data JoinType = InnerJoin | OuterJoin

class DoJoin (flag :: JoinType) t b out | flag t b -> out where
  doJoin :: (MonadSystem w m) => (a -> out -> c) -> Join m a t b -> Query m c

instance DoJoin InnerJoin Maybe b b where
  doJoin f (Join a f') =
    qmapMaybeM
      ( \e a -> do
          r <- f' e a
          case r of
            Nothing -> pure Nothing
            Just r -> pure $ Just $ f a r
      )
      a

instance DoJoin InnerJoin List (b) [b] where
  doJoin f (Join a f') =
    qmapMaybeM
      ( \e a -> do
          r <- f' e a
          case r of
            [] -> pure Nothing
            r -> pure $ Just $ f a r
      )
      a

instance DoJoin OuterJoin Maybe b (Maybe b) where
  doJoin f (Join a f') =
    qmapMaybeM
      ( \e a -> do
          r <- f' e a
          pure $ Just $ f a r
      )
      a

instance DoJoin OuterJoin List b [b] where
  doJoin f (Join a f') =
    qmapMaybeM
      ( \e a -> do
          r <- f' e a
          pure $ Just $ f a r
      )
      a

qjoin :: (MonadSystem w m, DoJoin InnerJoin t b out) => (a -> out -> c) -> Join m a t b -> Query m c
qjoin = doJoin @InnerJoin

qjoinOuter :: (MonadSystem w m, DoJoin OuterJoin t b out) => (a -> out -> c) -> Join m a t b -> Query m c
qjoinOuter = doJoin @OuterJoin

test :: System [(Name, [From Name])]
test = do
  mkQuery (C @Name)
    & qcross (==) (mkQuery (C @Name))
    & qjoin (,)
    & query

data Pos = Pos Int deriving (Component)

test' :: System [(Name, Maybe Pos)]
test' = do
  mkQuery (C @Name)
    & qextend (mkQuery (C @Pos))
    & qjoinOuter (,)
    & query