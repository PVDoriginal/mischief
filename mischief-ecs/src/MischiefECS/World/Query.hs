{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MischiefECS.World.Query where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Data
import Data.Foldable hiding (and)
import Data.IORef
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Internal
import MischiefECS.World.Par
import Prelude hiding (and)

class QueryData qd where
  types :: Proxy qd -> Set TypeRep

instance {-# OVERLAPPABLE #-} (Component c) => QueryData c where
  types :: (Component c) => Proxy c -> Set TypeRep
  types = Set.singleton . typeRep

instance (QueryData a0, QueryData a1) => QueryData (a0, a1) where
  types :: (QueryData a0, QueryData a1) => Proxy (a0, a1) -> Set TypeRep
  types _ = Set.union (types $ Proxy @a0) (types $ Proxy @a1)

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

  runQueryInternal :: Proxy qd -> [ArchetypeId] -> World -> IO [QueryOutput qd]
  default runQueryInternal ::
    (Component qd, QueryOutput qd ~ ComponentResult qd) =>
    Proxy qd -> [ArchetypeId] -> World -> IO [QueryOutput qd]
  runQueryInternal _ archetypes world = tryGetComponents @qd world archetypes

  outputEntity :: (Queryable qd) => Proxy qd -> QueryOutput qd -> Entity
  default outputEntity ::
    (Component qd, QueryOutput qd ~ ComponentResult qd) =>
    Proxy qd -> QueryOutput qd -> Entity
  outputEntity _ x = x.entity

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

    return $ zip r0 r1

  outputEntity :: (Queryable q0, Queryable q1) => Proxy (q0, q1) -> (QueryOutput q0, QueryOutput q1) -> Entity
  outputEntity _ (a, _) = outputEntity (Proxy @q0) a

instance Queryable Entity where
  type QueryOutput Entity = Entity

  runQueryEntity _ _ entity = pure (Just entity)
  runQueryInternal _ archetypes world = do
    results <- tryGetComponents @Entity world archetypes
    pure $ fmap (\cr -> cr.entity) results

  outputEntity _ x = x

runQuery :: forall qd. (Queryable qd) => Proxy qd -> QueryFilter -> World -> IO [QueryOutput qd]
runQuery query filter world =
  do
    components <- mapM (\c -> getComponentId c world.components) (Set.toList (types query))
    archetypes <- findMatchingArchetypes components world.archetypes

    let (otherFilter, archetypeFilter) = extractArchetypeFilters filter

    archetypes' <- filterM (\(components, _) -> filterArchetype archetypeFilter components world) archetypes

    outputs <- runQueryInternal query (map snd archetypes') world
    outputs' <- filterQuery @qd world otherFilter (map snd archetypes') (zip [0 ..] outputs)
    return $ map snd outputs'

query :: forall qd m w. (Queryable qd, MonadSystem w m) => m [QueryOutput qd]
query = do
  world <- asks getWorld
  liftIO $ runQuery (Proxy @qd) NoFilter world

entityQuery :: forall qd. (Queryable qd) => Entity -> System (Maybe (QueryOutput qd))
entityQuery entity = do
  world <- ask
  liftIO $ runQueryEntity (Proxy @qd) world entity

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

filterQuery :: forall qd. (Queryable qd) => World -> QueryFilter -> [ArchetypeId] -> [(Int, QueryOutput qd)] -> IO [(Int, QueryOutput qd)]
filterQuery _ NoFilter _ x = return x
filterQuery world (With x) _ outputs = do
  component <- liftIO $ getComponentId x world.components
  filterM
    ( \(_, output) -> do
        let entity = outputEntity (Proxy @qd) output
        components <- findComponentsOfEntity world entity
        case components of
          Nothing -> return True
          Just components ->
            return $ component `elem` components
    )
    outputs
filterQuery world (Without x) _ outputs = do
  component <- liftIO $ getComponentId x world.components
  filterM
    ( \(_, output) -> do
        let entity = outputEntity (Proxy @qd) output
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
  pointers <- liftIO $ readIORef world.entities.pointers

  let Tables t = world.tables
  tables <- liftIO $ readIORef t

  case Map.lookup entity pointers of
    Nothing -> return Nothing
    Just x -> do
      pointer <- liftIO $ readIORef x

      return $ case Map.lookup pointer.archetypeId tables of
        Nothing -> Nothing
        Just x -> Just x.components

instance Queryable Name

data QueryFilter = NoFilter | With TypeRep | Without TypeRep | And QueryFilter QueryFilter | Or QueryFilter QueryFilter | Changed TypeRep | Added TypeRep
  deriving (Show)

with :: forall qd. (QueryData qd) => QueryFilter
with = and' $ map With (Set.toList $ types (Proxy @qd))

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
  return $ component `elem` components
filterArchetype (Without x) components world = do
  component <- getComponentId x world.components
  return $ component `notElem` components
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
isArchetypeFilter (Without _) = True
isArchetypeFilter (a `And` b) = isArchetypeFilter a || isArchetypeFilter b
isArchetypeFilter (a `Or` b) = isArchetypeFilter a && isArchetypeFilter b
