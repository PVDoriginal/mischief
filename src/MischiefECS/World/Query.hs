{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.World.Query where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Proxy (Proxy (..))
import Data.Set
import Data.Set qualified as Set
import Data.Typeable (TypeRep, Typeable, typeRep)
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World

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

instance (Queryable q0, Queryable q1) => Queryable (q0, q1) where
  type QueryOutput (q0, q1) = (QueryOutput q0, QueryOutput q1)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity

    return $ liftA2 (,) r0 r1

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world

    return $ zip r0 r1

instance Queryable Entity where
  type QueryOutput Entity = Entity

  runQueryEntity _ _ entity = pure (Just entity)
  runQueryInternal _ archetypes world = do
    results <- tryGetComponents @Entity world archetypes
    pure $ fmap (\cr -> cr.entity) results

runQuery :: forall qd. (Queryable qd) => Proxy qd -> World -> IO [QueryOutput qd]
runQuery query world =
  do
    components <- mapM (\c -> getComponentId c world.components) (Set.toList (types query))
    archetypes <- findMatchingArchetypes components world.archetypes
    runQueryInternal query archetypes world

query :: forall qd r. (Queryable qd) => System [QueryOutput qd]
query = do
  world <- ask
  liftIO $ runQuery (Proxy @qd) world