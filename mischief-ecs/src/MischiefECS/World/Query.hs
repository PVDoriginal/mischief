{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MischiefECS.World.Query where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Data
import Data.Foldable hiding (and)
import Data.IORef
import Data.Kind
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Archetypes
import {-# SOURCE #-} MischiefECS.Archetypes.Graph
import MischiefECS.Components
import {-# SOURCE #-} MischiefECS.Components.Spawn
import MischiefECS.Entities
import MischiefECS.Entities.Internal
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Internal
import MischiefECS.World.Par
import MischiefECS.World.Utils
import Prelude hiding (and)

class QueryData qd where
  types :: Proxy qd -> Set TypeRep

instance {-# OVERLAPPABLE #-} (Component c) => QueryData c where
  types :: (Component c) => Proxy c -> Set TypeRep
  types = Set.singleton . typeRep

instance {-# OVERLAPPING #-} (Component c) => QueryData (R c) where
  types :: Proxy (R c) -> Set TypeRep
  types _ = Set.singleton $ typeRep $ Proxy @c

instance {-# OVERLAPPING #-} (Component c) => QueryData (Maybe c) where
  types :: (Component c) => Proxy (Maybe c) -> Set TypeRep
  types _ = Set.empty

instance {-# OVERLAPPING #-} (Component c) => QueryData (Has c) where
  types :: (Component c) => Proxy (Has c) -> Set TypeRep
  types _ = Set.empty

instance (QueryData a0, QueryData a1) => QueryData (a0, a1) where
  types :: (QueryData a0, QueryData a1) => Proxy (a0, a1) -> Set TypeRep
  types _ = Set.union (types $ Proxy @a0) (types $ Proxy @a1)

instance (QueryData a0, QueryData a1, QueryData a2) => QueryData (a0, a1, a2) where
  types :: (QueryData a0, QueryData a1, QueryData a2) => Proxy (a0, a1, a2) -> Set TypeRep
  types _ = Set.unions [types $ Proxy @a0, types $ Proxy @a1, types $ Proxy @a2]

class (QueryData qd) => Queryable qd where
  type QueryOutput qd
  type QueryOutput qd = ComponentResult qd

  runQueryEntity :: Proxy qd -> World -> Entity -> IO (Maybe (QueryOutput qd))
  default runQueryEntity ::
    (Component qd, QueryOutput qd ~ ComponentResult qd) =>
    Proxy qd -> World -> Entity -> IO (Maybe (QueryOutput qd))
  runQueryEntity _ world entity = do
    result <- tryGetEntityComponent @qd world entity
    pure $
      fmap (`ComponentResult` entity) result

  runQueryInternal :: Proxy qd -> [ArchetypeId] -> World -> IO [(Entity, QueryOutput qd)]
  default runQueryInternal ::
    (Component qd, QueryOutput qd ~ ComponentResult qd) =>
    Proxy qd -> [ArchetypeId] -> World -> IO [(Entity, QueryOutput qd)]
  runQueryInternal _ archetypes world = tryGetComponents @qd world archetypes

-- outputEntity :: (Queryable qd) => Proxy qd -> QueryOutput qd -> Entity
-- default outputEntity ::
--   (Component qd, QueryOutput qd ~ ComponentResult qd) =>
--   Proxy qd -> QueryOutput qd -> Entity
-- outputEntity _ x = x.entity

instance (Queryable q0, Queryable q1) => Queryable (q0, q1) where
  type QueryOutput (q0, q1) = (QueryOutput q0, QueryOutput q1)

  runQueryEntity :: (Queryable q0, Queryable q1) => Proxy (q0, q1) -> World -> Entity -> IO (Maybe (QueryOutput (q0, q1)))
  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity

    return $ liftA2 (,) r0 r1

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world

    return $ zipWith (\(e0, r0) (_, r1) -> (e0, (r0, r1))) r0 r1

-- outputEntity :: (Queryable q0, Queryable q1) => Proxy (q0, q1) -> (QueryOutput q0, QueryOutput q1) -> Entity
-- outputEntity _ (a, _) = outputEntity (Proxy @q0) a

instance (Queryable q0, Queryable q1, Queryable q2) => Queryable (q0, q1, q2) where
  type QueryOutput (q0, q1, q2) = (QueryOutput q0, QueryOutput q1, QueryOutput q2)

  runQueryEntity :: (Queryable q0, Queryable q1, Queryable q2) => Proxy (q0, q1, q2) -> World -> Entity -> IO (Maybe (QueryOutput (q0, q1, q2)))
  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity

    return $ do
      r0' <- r0
      r1' <- r1
      r2' <- r2
      return (r0', r1', r2')

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2)) -> (e0, (r0, r1, r2))) $ zip3 r0 r1 r2

-- outputEntity _ (a, _, _) = outputEntity (Proxy @q0) a

instance (Component c) => Queryable (R c) where
  type QueryOutput (R c) = RelationshipCollection c
  runQueryEntity _ = tryGetEntityRelationshipCollection
  runQueryInternal :: (Component c) => Proxy (R c) -> [ArchetypeId] -> World -> IO [(Entity, QueryOutput (R c))]
  runQueryInternal _ archetypes world = tryGetRelationshipCollections @c world archetypes

-- outputEntity _ x = x.entity

instance (Component c) => Queryable (Maybe c) where
  type QueryOutput (Maybe c) = (Maybe (ComponentResult c))
  runQueryEntity _ world entity = do
    res <- tryGetEntityComponent @c world entity
    case res of
      Nothing -> return Nothing
      Just x -> return $ Just $ Just $ ComponentResult x entity

  runQueryInternal _ archetypes world = tryGetComponentsMaybe @c world archetypes

-- outputEntity _ q = q.entity

data Has a = Has

instance (Component c) => Queryable (Has c) where
  type QueryOutput (Has c) = Bool

  runQueryEntity _ world entity = do
    list <- runQueryEntity (Proxy @(Maybe c)) world entity
    case list of
      Nothing -> return Nothing
      Just x -> return $ Just (isNothing x)

  runQueryInternal _ archetypes world = do
    list <- runQueryInternal (Proxy @(Maybe c)) archetypes world
    return $ map (\(e, x) -> (e, isNothing x)) list

-- outputEntity _ q = q.entity

instance Queryable Entity where
  type QueryOutput Entity = Entity

  runQueryEntity _ _ entity = pure (Just entity)
  runQueryInternal _ archetypes world = do
    results <- tryGetComponents @Entity world archetypes
    pure $ fmap (\cr -> (fst cr, fst cr)) results

-- outputEntity _ x = x

runQuery :: forall qd. (Queryable qd) => Proxy qd -> QueryFilter -> World -> IO [QueryOutput qd]
runQuery query filter world =
  do
    components <- mapM (\c -> getComponentId c world.components) (Set.toList (types query))
    archetypes <- findMatchingArchetypes (catMaybes components) world.components world.archetypes
    let (otherFilter, archetypeFilter) = extractArchetypeFilters filter

    archetypes' <- filterM (\(components, _) -> filterArchetype archetypeFilter components world) archetypes

    outputs <- runQueryInternal query (map snd archetypes') world
    outputs' <- filterQuery @qd world otherFilter (map snd archetypes') (zip [0 ..] outputs)
    return $ map (snd . snd) outputs'

query :: forall qd m w. (Queryable qd, MonadSystem w m) => m [QueryOutput qd]
query = do
  world <- asks getWorld
  liftIO $ runQuery (Proxy @qd) NoFilter world

entityQuery :: forall qd m w. (Queryable qd, MonadSystem w m) => Entity -> m (Maybe (QueryOutput qd))
entityQuery entity = do
  world <- asks getWorld
  liftIO $ runQueryEntity (Proxy @qd) world entity

get :: forall qd m w. (Queryable qd, MonadSystem w m) => Entity -> m (Maybe (QueryOutput qd))
get = entityQuery @qd

query' :: forall qd m w. (Queryable qd, MonadSystem w m) => QueryFilter -> m [QueryOutput qd]
query' filter = do
  world <- asks getWorld
  liftIO $ runQuery (Proxy @qd) filter world

single :: forall qd m w. (Queryable qd, MonadSystem w m) => m (Maybe (QueryOutput qd))
single = do
  res <- query @qd
  case res of
    [x] -> return $ Just x
    _ -> return Nothing

single' :: forall qd m w. (Queryable qd, MonadSystem w m) => QueryFilter -> m (Maybe (QueryOutput qd))
single' filter = do
  res <- query' @qd filter
  case res of
    [x] -> return $ Just x
    _ -> return Nothing

res :: forall c. (Queryable c, Component c) => System (Maybe (QueryOutput c))
res = do
  meta <- entityOf @c
  get @c meta

iter :: forall qd m w. (Queryable qd, MonadSystem w m) => (QueryOutput qd -> m ()) -> m ()
iter system = do
  res <- query @qd
  for_ res system

iter' :: forall qd m w. (Queryable qd, MonadSystem w m) => QueryFilter -> (QueryOutput qd -> m ()) -> m ()
iter' filter system = do
  res <- query' @qd filter
  for_ res system

parIter :: forall qd m w. (Queryable qd, MonadSystem w m) => (QueryOutput qd -> ParSystem ()) -> m ()
parIter system = do
  res <- query @qd
  parIterList res $ \chunk -> for_ chunk system

parIter' :: forall qd m w. (Queryable qd, MonadSystem w m) => QueryFilter -> (QueryOutput qd -> ParSystem ()) -> m ()
parIter' filter system = do
  res <- query' @qd filter
  parIterList res $ \chunk -> for_ chunk system

filterQuery :: forall qd. (Queryable qd) => World -> QueryFilter -> [ArchetypeId] -> [(Int, (Entity, QueryOutput qd))] -> IO [(Int, (Entity, QueryOutput qd))]
filterQuery _ NoFilter _ x = return x
filterQuery world (With x) _ outputs = do
  component <- liftIO $ getComponentId x world.components
  case component of
    Nothing -> return []
    Just component ->
      filterM
        ( \(_, (entity, _)) -> do
            components <- findComponentsOfEntity world entity
            case components of
              Nothing -> return True
              Just components ->
                return $ component `elem` components
        )
        outputs
filterQuery world (WithR x e) _ outputs = do
  component <- liftIO $ getComponentId x world.components
  case component of
    Nothing -> return []
    Just (ComponentId {id}) -> do
      let component = ComponentId {id, entity = Just e}
      filterM
        ( \(_, (entity, _)) -> do
            components <- findComponentsOfEntity world entity
            case components of
              Nothing -> return True
              Just components ->
                return $ component `elem` components
        )
        outputs
filterQuery world (Without x) _ outputs = do
  component <- liftIO $ getComponentId x world.components
  case component of
    Nothing -> return outputs
    Just component ->
      filterM
        ( \(_, (entity, _)) -> do
            components <- findComponentsOfEntity world entity
            case components of
              Nothing -> return True
              Just components ->
                return $ component `notElem` components
        )
        outputs
filterQuery world (Changed x) archetypes outputs = do
  res <- liftIO $ tryGetTicks x world archetypes
  return $
    filter
      ( \(index, _) ->
          let res' = res !! index
           in res'.changed >= world.lastSystemTick && res'.changed < world.currentSystemTick
      )
      outputs
filterQuery world (Added x) archetypes outputs = do
  res <- liftIO $ tryGetTicks x world archetypes
  return $
    filter
      ( \(index, _) ->
          let res' = res !! index
           in res'.added >= world.lastSystemTick && res'.added < world.currentSystemTick
      )
      outputs
filterQuery world (a `And` b) archetypes outputs = do
  res <- filterQuery @qd world a archetypes outputs
  filterQuery @qd world b archetypes res
filterQuery world (a `Or` b) archetypes outputs = do
  res1 <- filterQuery @qd world a archetypes outputs
  res2 <- filterQuery @qd world b archetypes outputs

  let res1' = map fst res1
  let res2' = map fst res2

  return $ filter (\(index, _) -> index `elem` res1' || index `elem` res2') outputs

findComponentsOfEntity :: World -> Entity -> IO (Maybe [ComponentId])
findComponentsOfEntity world entity = do
  pointer <- liftIO $ getPointer entity world.entities

  let Tables t = world.tables
  tables <- liftIO $ readIORef t

  case pointer of
    Nothing -> return Nothing
    Just x -> do
      pointer <- liftIO $ readIORef x

      return $ case Map.lookup pointer.archetypeId tables of
        Nothing -> Nothing
        Just x -> Just x.components

instance Queryable Name

instance Queryable ComponentType

instance (Component c) => Queryable (Meta c)

instance Queryable RequiredBy

instance Queryable Requires

instance Queryable DefaultValue

instance Queryable ComponentArchetypes

data QueryFilter
  = NoFilter
  | With TypeRep
  | WithR TypeRep Entity
  | Without TypeRep
  | And QueryFilter QueryFilter
  | Or QueryFilter QueryFilter
  | Changed TypeRep
  | Added TypeRep
  deriving (Show)

with :: forall qd. (QueryData qd) => QueryFilter
with = and' $ map With (Set.toList $ types (Proxy @qd))

withR :: forall c. (Component c) => Entity -> QueryFilter
withR = WithR (typeRep $ Proxy @c)

without :: forall qd. (QueryData qd) => QueryFilter
without = and' $ map Without (Set.toList $ types (Proxy @qd))

changed :: forall qd. (QueryData qd) => QueryFilter
changed = and' $ map Changed (Set.toList $ types (Proxy @qd))

added :: forall qd. (QueryData qd) => QueryFilter
added = and' $ map Added (Set.toList $ types (Proxy @qd))

and' :: [QueryFilter] -> QueryFilter
and' = foldr and NoFilter

and :: QueryFilter -> QueryFilter -> QueryFilter
and = And

or :: QueryFilter -> QueryFilter -> QueryFilter
or = Or

filterArchetype :: QueryFilter -> [ComponentId] -> World -> IO Bool
filterArchetype NoFilter _ _ = return True
filterArchetype (With x) components world = do
  component <- getComponentId x world.components
  return $ case component of
    Nothing -> False
    Just component -> component `elem` components
filterArchetype (WithR x e) components world = do
  component <- getComponentId x world.components
  case component of
    Nothing -> pure False
    Just (ComponentId {id}) -> do
      let component = ComponentId {id, entity = Just e}
      return $ component `elem` components
filterArchetype (Without x) components world = do
  component <- getComponentId x world.components
  return $ case component of
    Nothing -> True
    Just component -> component `notElem` components
filterArchetype (a `And` b) c w = do
  x <- filterArchetype a c w
  y <- filterArchetype b c w
  return $ x && y
filterArchetype (a `Or` b) c w = do
  x <- filterArchetype a c w
  y <- filterArchetype b c w
  return $ x || y
filterArchetype _ _ _ = pure True

-- | Extracts archetype-level filters from the bigger filter where possible, to be applied at the start of querying for better performance.
extractArchetypeFilters :: QueryFilter -> (QueryFilter, QueryFilter)
extractArchetypeFilters NoFilter = (NoFilter, NoFilter)
extractArchetypeFilters (With x) = (NoFilter, With x)
extractArchetypeFilters (WithR x e) = (NoFilter, WithR x e)
extractArchetypeFilters (Changed x) = (Changed x, NoFilter)
extractArchetypeFilters (Added x) = (Added x, NoFilter)
extractArchetypeFilters (Without x) = (NoFilter, Without x)
extractArchetypeFilters (a `And` b) = (filter1 `And` filter2, res1 `And` res2)
  where
    (filter1, res1) = extractArchetypeFilters a
    (filter2, res2) = extractArchetypeFilters b
extractArchetypeFilters (a `Or` b) =
  if isArchetypeFilter a && isArchetypeFilter b
    then
      (filter1 `Or` filter2, res1 `Or` res2)
    else
      (a `Or` b, NoFilter)
  where
    (filter1, res1) = extractArchetypeFilters a
    (filter2, res2) = extractArchetypeFilters b

isArchetypeFilter :: QueryFilter -> Bool
isArchetypeFilter NoFilter = True
isArchetypeFilter (Changed _) = False
isArchetypeFilter (Added _) = False
isArchetypeFilter (With _) = True
isArchetypeFilter (WithR _ _) = True
isArchetypeFilter (Without _) = True
isArchetypeFilter (a `And` b) = isArchetypeFilter a || isArchetypeFilter b
isArchetypeFilter (a `Or` b) = isArchetypeFilter a && isArchetypeFilter b
