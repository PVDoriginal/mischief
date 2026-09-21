{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.World.Query where

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import Data.Traversable
import Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.World
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Utils
import Prelude hiding (and)

findArchetypes :: forall qd m w output. (Queryable qd output, MonadSystem w m) => qd -> m [([ComponentId], ArchetypeId)]
findArchetypes query = do
  world <- unsafeGetWorld
  components <-
    liftIO $
      mapM
        ( \(c, t) -> do
            c <- getComponentId c world.components
            return $
              fmap
                ( \c ->
                    case t of
                      CompQ -> (c, ComponentQuery)
                      RelQ -> (c, RelationshipQueryAny)
                      RelQ' entity -> (setCompIdTarget (Just entity) c, RelationshipQuery)
                )
                c
        )
        (Set.toList (queryTypes query))

  findMatchingArchetypes (catMaybes components) world.archetypes

-- runQuery :: forall qd m w output a. (Queryable qd output, MonadSystem w m) => qd -> QueryFilter -> World -> m [output]
-- runQuery query filter world =
--   do
--     archetypes <- findArchetypes query
--     -- let (otherFilter, archetypeFilter) = extractArchetypeFilters $ preprocessFilter filter

--     -- archetypes' <- filterM (\(components, _) -> liftIO $ (filterArchetype . preprocessFilter) archetypeFilter components world) archetypes

--     -- outputs <- liftIO $ runQueryInternal query (map snd archetypes') world
--     -- outputs' <- filterM (\(e, b, _) -> (&& b) <$> filterQuery (preprocessFilter otherFilter) e) outputs
--     -- return $ map (\(_, _, o) -> o) outputs'
--     undefined

-- query :: forall qd output m w. (Queryable qd output, MonadSystem w m) => qd -> m [output]
-- query qd = do
--   world <- unsafeGetWorld
--   runQuery qd NoFilter world

entityQuery :: forall qd output m w. (Queryable qd output, MonadSystem w m) => qd -> Entity -> m (Maybe output)
entityQuery qd entity = do
  world <- unsafeGetWorld
  liftIO $ runQueryEntity qd world entity

-- get :: forall qd m w out. (Queryable qd out, MonadSystem w m) => qd -> Entity -> m (Maybe out)
-- get = entityQuery

-- get' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> Entity -> m (Maybe out)
-- get' qd qf entity = do
--   b <- filterQuery (preprocessFilter $ collect qf) entity
--   if b
--     then
--       entityQuery qd entity
--     else
--       pure Nothing

-- query' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> m [out]
-- query' qd filter = do
--   world <- unsafeGetWorld
--   runQuery qd (collect filter) world

-- single :: forall qd m w out. (Queryable qd out, MonadSystem w m) => qd -> m (Maybe out)
-- single qd = do
--   res <- query qd
--   case res of
--     [x] -> return $ Just x
--     _ -> return Nothing

-- single' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> m (Maybe out)
-- single' qd filter = do
--   res <- query' qd filter
--   case res of
--     [x] -> return $ Just x
--     _ -> return Nothing

-- iter :: forall qd m w. (Queryable qd, MonadSystem w m) => (QueryOutput qd -> m ()) -> m ()
-- iter system = do
--   res <- query @qd
--   for_ res system

-- iter' :: forall qd m w. (Queryable qd, MonadSystem w m) => QueryFilter -> (QueryOutput qd -> m ()) -> m ()
-- iter' filter system = do
--   res <- query' @qd filter
--   for_ res system

-- parIter :: forall qd m w. (Queryable qd, MonadSystem w m) => (QueryOutput qd -> ParSystem ()) -> m ()
-- parIter system = do
--   res <- query @qd
--   parIterList res $ \chunk -> for_ chunk system

class GetComponentId c where
  getComponentId' :: (MonadSystem w m) => c -> FilterComponent

tryMetaLocal :: forall c m w. (Component c, MonadSystem w m) => m (Maybe Entity)
tryMetaLocal = do
  world <- unsafeGetWorld
  component <- liftIO $ getComponentId (typeRep $ Proxy @c) world.components
  return $ fmap (\(ComponentId (# id, _ #)) -> Entity (# id, 0## #)) component

--   getComponentId' _ =

-- instance (Component c) => GetComponentId (R c Entity) where
--   getComponentId' (R e) = fmap (\(Entity (# id, _ #)) -> ComponentId (# id, Just e #)) <$
-- instance {-# OVERLAPPING #-} (Component c) => GetComponentId (C c) where> tryMetaLocal @c

-- instance (GetResultComponentId' (IsComp c) (Result c)) => GetResultComponentId (Result c) where
--   getResultComponentId = getResultComponentId' @(IsComp c)

addedChanged :: forall m w. (MonadSystem w m) => (ComponentTicks -> Tick -> Tick -> Bool) -> FilterComponent -> Entity -> m Bool
addedChanged f (FilterComponent (c, Nothing, Nothing)) e = do
  world <- unsafeGetWorld
  id <- liftIO $ getComponentId c world.components
  case id of
    Nothing -> return False
    Just id -> do
      ticks <- liftIO $ tryGetEntityTicks e id world
      case ticks of
        Nothing -> return False
        Just ticks -> do
          (lastSystemTick, currentSystemTick) <- liftIO $ getSystemTicksInternal world
          return $ f ticks lastSystemTick currentSystemTick
addedChanged f (FilterComponent (c, Just entity, Nothing)) e = do
  world <- unsafeGetWorld
  id <- liftIO $ getComponentId c world.components
  case id of
    Nothing -> return False
    Just (ComponentId (# id, _ #)) -> do
      ticks <- liftIO $ tryGetEntityTicks e (ComponentId (# id, Just entity #)) world
      case ticks of
        Nothing -> return False
        Just ticks -> do
          (lastSystemTick, currentSystemTick) <- liftIO $ getSystemTicksInternal world
          return $ f ticks lastSystemTick currentSystemTick
addedChanged f (FilterComponent (c, _, Just _)) e = do
  world <- unsafeGetWorld
  id <- liftIO $ getComponentId c world.components
  case id of
    Nothing -> return False
    Just (ComponentId (# id, _ #)) -> do
      Just (ComponentType (_ :: Proxy a)) <- liftIO $ runQueryEntity (C @ComponentType) world (Entity (# id, 0## #))
      rels <- single $ mkGet e (R' @a Any)
      case rels of
        Nothing -> pure False
        Just rels -> do
          and
            <$> for
              rels
              ( \(Rel _ target) -> do
                  addedChanged f (FilterComponent (c, Just target, Nothing)) e
              )

-- ticks <- liftIO $ tryGetEntityTicks e (ComponentId (# id, Just entity #)) world
-- case ticks of
--   Nothing -> return False
--   Just ticks -> do
--     (lastSystemTick, currentSystemTick) <- liftIO $ getSystemTicksInternal world
--     return $ f ticks lastSystemTick currentSystemTick

added :: forall c m w. (MonadSystem w m, ToFilterComponent c) => c -> Entity -> m Bool
added x = addedChanged qfAddedF (toFilterComponent x)

changed :: forall c m w. (MonadSystem w m, ToFilterComponent c) => c -> Entity -> m Bool
changed x = addedChanged qfChangedF (toFilterComponent x)

has :: forall c m w. (MonadSystem w m, ToFilterComponent c) => c -> Entity -> m Bool
has c e = do
  world <- unsafeGetWorld
  liftIO $ filterEntity (With c) world e

-- newtype Query a = Query [(Entity, a)]

data Query m a where
  BuildQuery :: (Queryable qd out) => qd -> QueryFilter ArchetypeFilter -> Maybe [Entity] -> Query m out
  MapQuery :: Query m b -> (Entity -> b -> m a) -> Query m a
  FilterQuery :: Query m a -> (Entity -> a -> m Bool) -> Query m a
  PairEntityQuery :: Query m a -> Query m (Entity, a)
  DoQuery :: Query m a -> (Entity -> a -> m b) -> Query m a
  FoldQuery :: Query m a -> Entity -> (From a -> b -> b) -> b -> Query m b
  PureQuery :: a -> Query m a
  AppQuery :: Query m (a -> b) -> Query m a -> Query m b
  BindQuery :: Query m a -> (a -> Query m b) -> Query m b
  EmptyQuery :: Query m a

instance (MonadSystem w m) => Functor (Query m) where
  fmap :: (a -> b) -> Query m a -> Query m b
  fmap f a = MapQuery a (\_ x -> pure $ f x)

instance (MonadSystem w m) => Applicative (Query m) where
  pure :: a -> Query m a
  pure = PureQuery
  (<*>) :: Query m (a -> b) -> Query m a -> Query m b
  (<*>) = AppQuery

instance (MonadSystem w m) => Monad (Query m) where
  (>>=) :: (MonadSystem w m) => Query m a -> (a -> Query m b) -> Query m b
  (>>=) = BindQuery

-- grun :: (MonadSystem w m) => Entity -> Query m out -> m (Maybe (Entity, out))
-- grun entity (BuildQuery qd qf e) = do
--   world <- unsafeGetWorld
--   b <- liftIO $ filterEntity qf world entity
--   if b then fmap (entity,) <$> entityQuery qd entity else pure Nothing
-- grun entity (MapQuery a f) = do
--   x <- grun entity a
--   case x of
--     Nothing -> pure Nothing
--     Just (e, x) -> fmap (e,) . Just <$> f entity x
-- grun entity (FilterQuery a f) = do
--   x <- grun entity a
--   case x of
--     Nothing -> pure Nothing
--     Just (e, x) -> do
--       b <- f entity x
--       if b then pure $ Just (e, x) else pure Nothing
-- grun entity (DoQuery a f) = do
--   x <- grun entity a
--   for_ x $ uncurry f
--   pure x
-- grun _ (FoldQuery a e f i) = do
--   x <- map snd <$> qrun a
--   let x' = foldr f i x
--   pure $ Just (e, x')
-- grun _ (PureQuery a) = pure $ Just (Entity (# 0##, 0## #), a)
-- grun entity (AppQuery f a) = do
--   f <- grun entity f
--   a <- grun entity a
--   case (,) <$> f <*> a of
--     Nothing -> pure Nothing
--     Just ((_, f), (e', a)) -> pure $ Just (e', f a)
-- grun entity (BindQuery f a) = do
--   x <- grun entity f
--   case x of
--     Nothing -> pure Nothing
--     Just (_, x) -> grun entity (a x)

-- get :: (MonadSystem w m) => Entity -> Query m out -> m (Maybe out)
-- get e a = fmap snd <$> grun e a

-- get_ :: (MonadSystem w m) => Entity -> Query m out -> m ()
-- get_ a b = void $ get a b

qrun :: (MonadSystem w m) => Query m out -> m [(Entity, out)]
qrun (BuildQuery qd qf Nothing) = do
  world <- unsafeGetWorld
  archetypes' <- findArchetypes qd
  archetypes <- liftIO (filterM (filterArchetype qf world . fst) archetypes')
  x <- liftIO $ runQueryInternal (E, qd) (map snd archetypes) world
  pure $ mapMaybe (\case (_, False, _) -> Nothing; (_, True, x) -> Just x) x
qrun (BuildQuery qd qf (Just [entity])) = do
  world <- unsafeGetWorld
  b <- liftIO $ filterEntity qf world entity
  if not b
    then pure []
    else do
      e <- entityQuery qd entity
      case e of
        Nothing -> pure []
        Just e -> pure [(entity, e)]
qrun (BuildQuery qd qf (Just entities)) = concat <$> mapM (\e -> qrun (BuildQuery qd qf (Just [e]))) entities
qrun (MapQuery a f) = do
  x <- qrun a
  mapM (\(e, x) -> (e,) <$> f e x) x
qrun (FilterQuery a f) = do
  x <- qrun a
  filterM (uncurry f) x
qrun (PairEntityQuery a) = do
  x <- qrun a
  pure $ map (\(e, a) -> (e, (e, a))) x
qrun (DoQuery a f) = do
  x <- qrun a
  for_ x (uncurry f)
  pure x
qrun (FoldQuery a e f i) = do
  a <- map (uncurry From) <$> qrun a
  pure [(e, foldr f i a)]
qrun (PureQuery a) = pure [(Entity (# 0##, 0## #), a)]
qrun (AppQuery f a) = do
  f <- qrun f
  a <- qrun a
  pure $ catMaybes $ [tryJoin a f | a <- a, f <- f]
qrun (BindQuery a f) = do
  x <- qrun a
  -- concatMap (\(a, b) -> map (a,) b) <$> traverse (\(e, x) -> do (e,) <$> query (f x)) x
  concat
    <$> traverse
      ( \(_, x) -> do
          qrun (f x)
      )
      x
qrun EmptyQuery = pure []

tryJoin :: (Entity, a) -> (Entity, a -> b) -> Maybe (Entity, b)
tryJoin (Entity (# 0##, 0## #), a) (e, f) = Just (e, f a)
tryJoin (e, a) (Entity (# 0##, 0## #), f) = Just (e, f a)
tryJoin (e, a) (e', f)
  | e == e' = Just (e, f a)
  | otherwise = Nothing

query :: (MonadSystem w m) => Query m out -> m [out]
query a = map snd <$> qrun a

query_ :: (MonadSystem w m) => Query m out -> m ()
query_ a = void $ qrun a

single :: (MonadSystem w m) => Query m out -> m (Maybe out)
single a = do
  x <- query a
  case x of
    [x] -> pure $ Just x
    _ -> pure Nothing

mkQuery :: (Queryable qd out) => qd -> Query m out
mkQuery x = mkQuery' x NoFilter

mkQuery' :: (Queryable qd out) => qd -> QueryFilter ArchetypeFilter -> Query m out
mkQuery' a b = BuildQuery a b Nothing

mkGet :: (Queryable qd out) => Entity -> qd -> Query m out
mkGet x a = mkGet' x a NoFilter

mkGet' :: (Queryable qd out) => Entity -> qd -> QueryFilter ArchetypeFilter -> Query m out
mkGet' a b c = BuildQuery b c (Just [a])