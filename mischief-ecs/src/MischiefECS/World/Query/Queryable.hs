module MischiefECS.World.Query.Queryable where

import Control.Applicative
import Data.Data
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query.QueryData
import MischiefECS.World.Utils

class (QueryData qd) => Queryable qd where
  type QueryOutput qd
  type QueryOutput qd = Result qd

  runQueryEntity :: Proxy qd -> World -> Entity -> IO (Maybe (QueryOutput qd))
  default runQueryEntity ::
    (Component qd, QueryOutput qd ~ Result qd) =>
    Proxy qd -> World -> Entity -> IO (Maybe (QueryOutput qd))
  runQueryEntity _ world entity = do
    result <- tryGetEntityComponent @qd world entity
    return $ case result of
      Just (Just res) -> Just $ Result (res, entity)
      _ -> Nothing

  -- pure $
  --   fmap (`Result` entity) result

  runQueryInternal :: Proxy qd -> [ArchetypeId] -> World -> IO [(Entity, QueryOutput qd)]
  default runQueryInternal ::
    (Component qd, QueryOutput qd ~ Result qd) =>
    Proxy qd -> [ArchetypeId] -> World -> IO [(Entity, QueryOutput qd)]
  runQueryInternal _ archetypes world = tryGetComponents @qd world archetypes

-- outputEntity :: (Queryable qd) => Proxy qd -> QueryOutput qd -> Entity
-- default outputEntity ::
--   (Component qd, QueryOutput qd ~ Result qd) =>
--   Proxy qd -> QueryOutput qd -> Entity
-- outputEntity _ x = x.entity

-- outputEntity _ (a, _, _) = outputEntity (Proxy @q0) a

instance (Component c) => Queryable (Rel c) where
  type QueryOutput (Rel c) = [RelResult c]
  runQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Just (Just x) -> Just x
      _ -> Nothing

  runQueryInternal :: (Component c) => Proxy (Rel c) -> [ArchetypeId] -> World -> IO [(Entity, QueryOutput (Rel c))]
  runQueryInternal _ archetypes world = tryGetRelCollections @c world archetypes

-- outputEntity _ x = x.entity

instance (Component c) => Queryable (Maybe c) where
  type QueryOutput (Maybe c) = (Maybe (Result c))
  runQueryEntity _ world entity = do
    res <- tryGetEntityComponent @c world entity
    case res of
      Nothing -> return Nothing
      Just Nothing -> return $ Just Nothing
      Just (Just x) -> return $ Just $ Just $ Result (x, entity)

  runQueryInternal _ archetypes world = tryGetComponentsMaybe @c world archetypes

instance (Component c) => Queryable (MaybeRel c) where
  type QueryOutput (MaybeRel c) = [RelResult c]
  runQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just []
      Just (Just x) -> Just x

  -- This looks the same as normal 'Rel', but, through QueryData, it will also include archetypes
  -- which don't have this relationship.
  runQueryInternal _ archetypes world = tryGetRelCollections @c world archetypes

instance (Component c) => Queryable (Has c) where
  type QueryOutput (Has c) = Bool

  runQueryEntity _ world entity = do
    list <- runQueryEntity (Proxy @(Maybe c)) world entity
    case list of
      Nothing -> return Nothing
      Just x -> return $ Just (isJust x)

  runQueryInternal _ archetypes world = do
    list <- runQueryInternal (Proxy @(Maybe c)) archetypes world
    return $ map (\(e, x) -> (e, isJust x)) list

instance (Component c) => Queryable (HasRel c) where
  type QueryOutput (HasRel c) = Bool

  runQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just False
      Just (Just _) -> Just True

  runQueryInternal _ archetypes world = do
    res <- tryGetRelCollections @c world archetypes
    return $ map (\(e, c) -> (e, null c)) res

-- outputEntity _ q = q.entity

instance Queryable Entity where
  type QueryOutput Entity = Entity

  runQueryEntity _ _ entity = pure (Just entity)
  runQueryInternal _ archetypes world = do
    results <- tryGetComponents @Entity world archetypes
    pure $ fmap (\cr -> (fst cr, fst cr)) results

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1) => Queryable (q0, q1) where
  type QueryOutput (q0, q1) = (QueryOutput q0, QueryOutput q1)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity

    return $ (,) <$> r0 <*> r1

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world

    return $ map (\((e0, r0), (_, r1)) -> (e0, (r0, r1))) $ getZipList $ (,) <$> ZipList r0 <*> ZipList r1

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2) => Queryable (q0, q1, q2) where
  type QueryOutput (q0, q1, q2) = (QueryOutput q0, QueryOutput q1, QueryOutput q2)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity

    return $ (,,) <$> r0 <*> r1 <*> r2

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2)) -> (e0, (r0, r1, r2))) $ getZipList $ (,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3) => Queryable (q0, q1, q2, q3) where
  type QueryOutput (q0, q1, q2, q3) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity

    return $ (,,,) <$> r0 <*> r1 <*> r2 <*> r3

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3)) -> (e0, (r0, r1, r2, r3))) $ getZipList $ (,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4) => Queryable (q0, q1, q2, q3, q4) where
  type QueryOutput (q0, q1, q2, q3, q4) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity

    return $ (,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4)) -> (e0, (r0, r1, r2, r3, r4))) $ getZipList $ (,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5) => Queryable (q0, q1, q2, q3, q4, q5) where
  type QueryOutput (q0, q1, q2, q3, q4, q5) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity

    return $ (,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5)) -> (e0, (r0, r1, r2, r3, r4, r5))) $ getZipList $ (,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6) => Queryable (q0, q1, q2, q3, q4, q5, q6) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity

    return $ (,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6)) -> (e0, (r0, r1, r2, r3, r4, r5, r6))) $ getZipList $ (,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity

    return $ (,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7))) $ getZipList $ (,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7, Queryable q8) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7, q8) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7, QueryOutput q8)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity
    r8 <- runQueryEntity (Proxy @q8) world entity

    return $ (,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world
    r8 <- runQueryInternal (Proxy @q8) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8))) $ getZipList $ (,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7, Queryable q8, Queryable q9) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7, QueryOutput q8, QueryOutput q9)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity
    r8 <- runQueryEntity (Proxy @q8) world entity
    r9 <- runQueryEntity (Proxy @q9) world entity

    return $ (,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world
    r8 <- runQueryInternal (Proxy @q8) archetypes world
    r9 <- runQueryInternal (Proxy @q9) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9))) $ getZipList $ (,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7, Queryable q8, Queryable q9, Queryable q10) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7, QueryOutput q8, QueryOutput q9, QueryOutput q10)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity
    r8 <- runQueryEntity (Proxy @q8) world entity
    r9 <- runQueryEntity (Proxy @q9) world entity
    r10 <- runQueryEntity (Proxy @q10) world entity

    return $ (,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world
    r8 <- runQueryInternal (Proxy @q8) archetypes world
    r9 <- runQueryInternal (Proxy @q9) archetypes world
    r10 <- runQueryInternal (Proxy @q10) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10))) $ getZipList $ (,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7, Queryable q8, Queryable q9, Queryable q10, Queryable q11) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7, QueryOutput q8, QueryOutput q9, QueryOutput q10, QueryOutput q11)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity
    r8 <- runQueryEntity (Proxy @q8) world entity
    r9 <- runQueryEntity (Proxy @q9) world entity
    r10 <- runQueryEntity (Proxy @q10) world entity
    r11 <- runQueryEntity (Proxy @q11) world entity

    return $ (,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world
    r8 <- runQueryInternal (Proxy @q8) archetypes world
    r9 <- runQueryInternal (Proxy @q9) archetypes world
    r10 <- runQueryInternal (Proxy @q10) archetypes world
    r11 <- runQueryInternal (Proxy @q11) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11))) $ getZipList $ (,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7, Queryable q8, Queryable q9, Queryable q10, Queryable q11, Queryable q12) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7, QueryOutput q8, QueryOutput q9, QueryOutput q10, QueryOutput q11, QueryOutput q12)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity
    r8 <- runQueryEntity (Proxy @q8) world entity
    r9 <- runQueryEntity (Proxy @q9) world entity
    r10 <- runQueryEntity (Proxy @q10) world entity
    r11 <- runQueryEntity (Proxy @q11) world entity
    r12 <- runQueryEntity (Proxy @q12) world entity

    return $ (,,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world
    r8 <- runQueryInternal (Proxy @q8) archetypes world
    r9 <- runQueryInternal (Proxy @q9) archetypes world
    r10 <- runQueryInternal (Proxy @q10) archetypes world
    r11 <- runQueryInternal (Proxy @q11) archetypes world
    r12 <- runQueryInternal (Proxy @q12) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11), (_, r12)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12))) $ getZipList $ (,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7, Queryable q8, Queryable q9, Queryable q10, Queryable q11, Queryable q12, Queryable q13) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7, QueryOutput q8, QueryOutput q9, QueryOutput q10, QueryOutput q11, QueryOutput q12, QueryOutput q13)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity
    r8 <- runQueryEntity (Proxy @q8) world entity
    r9 <- runQueryEntity (Proxy @q9) world entity
    r10 <- runQueryEntity (Proxy @q10) world entity
    r11 <- runQueryEntity (Proxy @q11) world entity
    r12 <- runQueryEntity (Proxy @q12) world entity
    r13 <- runQueryEntity (Proxy @q13) world entity

    return $ (,,,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12 <*> r13

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world
    r8 <- runQueryInternal (Proxy @q8) archetypes world
    r9 <- runQueryInternal (Proxy @q9) archetypes world
    r10 <- runQueryInternal (Proxy @q10) archetypes world
    r11 <- runQueryInternal (Proxy @q11) archetypes world
    r12 <- runQueryInternal (Proxy @q12) archetypes world
    r13 <- runQueryInternal (Proxy @q13) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11), (_, r12), (_, r13)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13))) $ getZipList $ (,,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12 <*> ZipList r13

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1, Queryable q2, Queryable q3, Queryable q4, Queryable q5, Queryable q6, Queryable q7, Queryable q8, Queryable q9, Queryable q10, Queryable q11, Queryable q12, Queryable q13, Queryable q14) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14) where
  type QueryOutput (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14) = (QueryOutput q0, QueryOutput q1, QueryOutput q2, QueryOutput q3, QueryOutput q4, QueryOutput q5, QueryOutput q6, QueryOutput q7, QueryOutput q8, QueryOutput q9, QueryOutput q10, QueryOutput q11, QueryOutput q12, QueryOutput q13, QueryOutput q14)

  runQueryEntity _ world entity = do
    r0 <- runQueryEntity (Proxy @q0) world entity
    r1 <- runQueryEntity (Proxy @q1) world entity
    r2 <- runQueryEntity (Proxy @q2) world entity
    r3 <- runQueryEntity (Proxy @q3) world entity
    r4 <- runQueryEntity (Proxy @q4) world entity
    r5 <- runQueryEntity (Proxy @q5) world entity
    r6 <- runQueryEntity (Proxy @q6) world entity
    r7 <- runQueryEntity (Proxy @q7) world entity
    r8 <- runQueryEntity (Proxy @q8) world entity
    r9 <- runQueryEntity (Proxy @q9) world entity
    r10 <- runQueryEntity (Proxy @q10) world entity
    r11 <- runQueryEntity (Proxy @q11) world entity
    r12 <- runQueryEntity (Proxy @q12) world entity
    r13 <- runQueryEntity (Proxy @q13) world entity
    r14 <- runQueryEntity (Proxy @q14) world entity

    return $ (,,,,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12 <*> r13 <*> r14

  runQueryInternal _ archetypes world = do
    r0 <- runQueryInternal (Proxy @q0) archetypes world
    r1 <- runQueryInternal (Proxy @q1) archetypes world
    r2 <- runQueryInternal (Proxy @q2) archetypes world
    r3 <- runQueryInternal (Proxy @q3) archetypes world
    r4 <- runQueryInternal (Proxy @q4) archetypes world
    r5 <- runQueryInternal (Proxy @q5) archetypes world
    r6 <- runQueryInternal (Proxy @q6) archetypes world
    r7 <- runQueryInternal (Proxy @q7) archetypes world
    r8 <- runQueryInternal (Proxy @q8) archetypes world
    r9 <- runQueryInternal (Proxy @q9) archetypes world
    r10 <- runQueryInternal (Proxy @q10) archetypes world
    r11 <- runQueryInternal (Proxy @q11) archetypes world
    r12 <- runQueryInternal (Proxy @q12) archetypes world
    r13 <- runQueryInternal (Proxy @q13) archetypes world
    r14 <- runQueryInternal (Proxy @q14) archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11), (_, r12), (_, r13), (_, r14)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14))) $ getZipList $ (,,,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12 <*> ZipList r13 <*> ZipList r14
