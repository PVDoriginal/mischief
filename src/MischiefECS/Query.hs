module MischiefECS.Query where

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

class (QueryData qd) => Queryable qd r | qd -> r where
  runQueryEntity :: Proxy qd -> World -> Entity -> IO (Maybe r)
  runQueryInternal :: Proxy qd -> [ArchetypeId] -> World -> IO [r]

instance {-# OVERLAPPABLE #-} (Component c) => Queryable c r where
  runQueryEntity :: (Component c) => Proxy c -> World -> Entity -> IO (Maybe (ComponentResult c))
  runQueryEntity _ world entity = do
    result <- tryGetEntityComponent c world entity
    return $ case result of
      Nothing -> Nothing
      Just result -> Just $ ComponentResult result entity

  runQueryInternal :: (Component c) => Proxy c -> [ArchetypeId] -> World -> IO [ComponentResult c]
  runQueryInternal _ archetypes world = tryGetComponents c world archetypes

instance {-# OVERLAPPING #-} (Queryable q0 r0, Queryable q1 r1) => Queryable (q0, q1) (r0, r1) where
  runQueryEntity :: (Queryable q0 r0, Queryable q1 r1) => Proxy (q0, q1) -> World -> Entity -> IO (Maybe (r0, r1))
  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity

    -- let res = do
    --         r0 <- r0
    --         r1 <- r1
    --         return (r0, r1)
    -- here you can actually do (,) <$> r0 <*> r1 using Maybe's applicative and functor instance
    -- or the slightly more readable (IMO) version
    -- liftA2 imo reads nicer but it's only upto 2 values, so if you had more you'd have to use the (,) <$> a1 <*> a2 <*> ... <*> aN version
    -- P.S.: liftA2 f r0 r1 = do { a <- r0; b <- r1; pure $ f a b }
    return $ liftA2 (,) r0 r1

  runQueryInternal :: (Queryable q0 r0, Queryable q1 r1) => Proxy (q0, q1) -> [ArchetypeId] -> World -> IO [(r0, r1)]
  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world

    return $ zip r0 r1

runQuery :: forall qd. forall r. (QueryData qd, Queryable qd r) => Proxy qd -> World -> IO [r]
runQuery query world =
  do
    components <- mapM (\c -> getComponentId c world.components) (Set.toList (types query))
    archetypes <- findMatchingArchetypes components world.archetypes
    runQueryInternal query archetypes world

query :: forall qd. forall r. (QueryData qd, Queryable qd r) => System [r]
query = do
  world <- ask
  liftIO $ runQuery (Proxy @qd) world