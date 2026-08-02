{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Query.Queryable where

import Control.Applicative
import Data.Bifunctor qualified
import Data.Data
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Mappable
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Utils

data TypeQuery = CompQ | RelQ | RelQ' Entity deriving (Eq, Ord, Show)

data MR a b = MR b

data Has a = Has

data HasR a b = HasR b

newtype Val a = Val a

data E = E

data C a = C

newtype R a b = R b

data Any = Any

data RelTarget = AnyTarget | RelTargets [Entities]

data M a = M

class Queryable qd output | qd -> output where
  runQueryEntity :: qd -> World -> Entity -> IO (Maybe output)
  runQueryInternal :: qd -> [ArchetypeId] -> World -> IO [(Entity, output)]
  queryTypes :: qd -> Set (TypeRep, TypeQuery)

data HTrue

data HFalse

type family IsComponentC c where
  IsComponentC Entity = HFalse
  IsComponentC a = HTrue

instance {-# OVERLAPPABLE #-} (Component c) => Queryable (C c) (Result c) where
  runQueryEntity _ world entity = do
    result <- tryGetEntityComponent @c world entity
    return $ case result of
      Just (Just res) -> Just $ Result (res, entity)
      _ -> Nothing

  runQueryInternal _ archetypes world = tryGetComponents @c world archetypes

  queryTypes _ = Set.singleton (typeRep (Proxy @c), CompQ)

-- instance {-# OVERLAPPING #-} Queryable (C Entity) Entity where
--   runQueryEntity _ _ entity = pure (Just entity)
--   runQueryInternal _ archetypes world = do
--     results <- tryGetComponents @Entity world archetypes
--     pure $ fmap (\cr -> (fst cr, fst cr)) results

--   queryTypes _ = Set.empty

instance (Component c) => Queryable (R c Any) [Result (Rel c)] where
  runQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Just (Just x) -> Just x
      _ -> Nothing

  runQueryInternal _ archetypes world = tryGetRelCollections @c world archetypes

  queryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance Queryable E Entity where
  runQueryEntity _ _ entity = return $ Just entity

  runQueryInternal _ archetypes world = map (\x -> (x, x)) <$> tryGetEntities world archetypes

  queryTypes _ = Set.empty

instance (Component c) => Queryable (R c Entity) (Result (Rel c)) where
  runQueryEntity (R target) world entity = do
    res <- tryGetEntityRel @c target world entity
    return $ case res of
      Just (Just x) -> Just $ Result (Rel x target, entity)
      _ -> Nothing

  runQueryInternal (R target) archetypes world = tryGetRels @c target world archetypes

  queryTypes (R target) = Set.singleton (typeRep $ Proxy @c, RelQ' target)

instance (Component c) => Queryable (M c) (Maybe (Result c)) where
  runQueryEntity _ world entity = do
    res <- tryGetEntityComponent @c world entity
    case res of
      Nothing -> return Nothing
      Just Nothing -> return $ Just Nothing
      Just (Just x) -> return $ Just $ Just $ Result (x, entity)

  runQueryInternal _ archetypes world = tryGetComponentsMaybe @c world archetypes

  queryTypes _ = Set.empty

instance (Component c) => Queryable (MR c Any) (Maybe [Result (Rel c)]) where
  runQueryEntity _ = tryGetEntityRelCollection @c

  runQueryInternal _ archetypes world = do
    x <- tryGetRelCollections @c world archetypes
    return $ flip map x $ \(e, x) ->
      case x of
        [] -> (e, Nothing)
        x -> (e, Just x)
  queryTypes _ = Set.empty

instance (Component c) => Queryable (MR c Entity) (Maybe (Result (Rel c))) where
  runQueryEntity (MR target) world entity = do
    res <- tryGetEntityRel @c target world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just Nothing
      Just (Just x) -> Just $ Just (Result (Rel x target, entity))

  runQueryInternal (MR target) archetypes world = tryGetRelsMaybe @c target world archetypes
  queryTypes _ = Set.empty

instance (Component c) => Queryable (Has c) Bool where
  runQueryEntity _ world entity = do
    list <- runQueryEntity (M @c) world entity
    case list of
      Nothing -> return Nothing
      Just x -> return $ Just (isJust x)

  runQueryInternal _ archetypes world = do
    list <- runQueryInternal (M @c) archetypes world
    return $ map (Data.Bifunctor.second isJust) list

  queryTypes _ = Set.empty

instance (Component c) => Queryable (HasR c Any) Bool where
  runQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just False
      Just (Just _) -> Just True

  runQueryInternal _ archetypes world = do
    res <- tryGetRelCollections @c world archetypes
    return $ map (Data.Bifunctor.second null) res

  queryTypes _ = Set.empty

instance (Component c) => Queryable (HasR c Entity) Bool where
  runQueryEntity (HasR target) world entity = do
    res <- tryGetEntityRel @c target world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just False
      Just (Just _) -> Just True

  runQueryInternal (HasR target) archetypes world = do
    res <- runQueryInternal (MR @c target) archetypes world
    return $ map (Data.Bifunctor.second isNothing) res

  queryTypes _ = Set.empty

instance (Queryable qd out, Mappable MapQueryVal out out') => Queryable (Val qd) out' where
  runQueryEntity (Val qd) b c = do
    x <- runQueryEntity qd b c
    return $ fmap (mapTuple @MapQueryVal) x
  runQueryInternal (Val qd) b c = do
    x <- runQueryInternal qd b c
    return $ map (\(a, b) -> (a, mapTuple @MapQueryVal b)) x

  queryTypes (Val qd) = queryTypes qd

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1) => Queryable (q0, q1) (o0, o1) where
  runQueryEntity (q0, q1) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity

    return $ (,) <$> r0 <*> r1

  runQueryInternal (q0, q1) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world

    return $ map (\((e0, r0), (_, r1)) -> (e0, (r0, r1))) $ getZipList $ (,) <$> ZipList r0 <*> ZipList r1

  queryTypes (q0, q1) = Set.unions [queryTypes q0, queryTypes q1]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2) => Queryable (q0, q1, q2) (o0, o1, o2) where
  runQueryEntity (q0, q1, q2) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity

    return $ (,,) <$> r0 <*> r1 <*> r2

  runQueryInternal (q0, q1, q2) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2)) -> (e0, (r0, r1, r2))) $ getZipList $ (,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2

  queryTypes (q0, q1, q2) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3) => Queryable (q0, q1, q2, q3) (o0, o1, o2, o3) where
  runQueryEntity (q0, q1, q2, q3) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity

    return $ (,,,) <$> r0 <*> r1 <*> r2 <*> r3

  runQueryInternal (q0, q1, q2, q3) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3)) -> (e0, (r0, r1, r2, r3))) $ getZipList $ (,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3

  queryTypes (q0, q1, q2, q3) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4) => Queryable (q0, q1, q2, q3, q4) (o0, o1, o2, o3, o4) where
  runQueryEntity (q0, q1, q2, q3, q4) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity

    return $ (,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4

  runQueryInternal (q0, q1, q2, q3, q4) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4)) -> (e0, (r0, r1, r2, r3, r4))) $ getZipList $ (,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4

  queryTypes (q0, q1, q2, q3, q4) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5) => Queryable (q0, q1, q2, q3, q4, q5) (o0, o1, o2, o3, o4, o5) where
  runQueryEntity (q0, q1, q2, q3, q4, q5) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity

    return $ (,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5

  runQueryInternal (q0, q1, q2, q3, q4, q5) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5)) -> (e0, (r0, r1, r2, r3, r4, r5))) $ getZipList $ (,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5

  queryTypes (q0, q1, q2, q3, q4, q5) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6) => Queryable (q0, q1, q2, q3, q4, q5, q6) (o0, o1, o2, o3, o4, o5, o6) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity

    return $ (,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6)) -> (e0, (r0, r1, r2, r3, r4, r5, r6))) $ getZipList $ (,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6

  queryTypes (q0, q1, q2, q3, q4, q5, q6) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7) (o0, o1, o2, o3, o4, o5, o6, o7) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity

    return $ (,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7))) $ getZipList $ (,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7, Queryable q8 o8) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8) (o0, o1, o2, o3, o4, o5, o6, o7, o8) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7, q8) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity
    r8 <- runQueryEntity q8 world entity

    return $ (,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7, q8) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world
    r8 <- runQueryInternal q8 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8))) $ getZipList $ (,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7, Queryable q8 o8, Queryable q9 o9) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9) (o0, o1, o2, o3, o4, o5, o6, o7, o8, o9) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity
    r8 <- runQueryEntity q8 world entity
    r9 <- runQueryEntity q9 world entity

    return $ (,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world
    r8 <- runQueryInternal q8 archetypes world
    r9 <- runQueryInternal q9 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9))) $ getZipList $ (,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8, queryTypes q9]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7, Queryable q8 o8, Queryable q9 o9, Queryable q10 o10) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) (o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity
    r8 <- runQueryEntity q8 world entity
    r9 <- runQueryEntity q9 world entity
    r10 <- runQueryEntity q10 world entity

    return $ (,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world
    r8 <- runQueryInternal q8 archetypes world
    r9 <- runQueryInternal q9 archetypes world
    r10 <- runQueryInternal q10 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10))) $ getZipList $ (,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8, queryTypes q9, queryTypes q10]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7, Queryable q8 o8, Queryable q9 o9, Queryable q10 o10, Queryable q11 o11) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11) (o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity
    r8 <- runQueryEntity q8 world entity
    r9 <- runQueryEntity q9 world entity
    r10 <- runQueryEntity q10 world entity
    r11 <- runQueryEntity q11 world entity

    return $ (,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world
    r8 <- runQueryInternal q8 archetypes world
    r9 <- runQueryInternal q9 archetypes world
    r10 <- runQueryInternal q10 archetypes world
    r11 <- runQueryInternal q11 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11))) $ getZipList $ (,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8, queryTypes q9, queryTypes q10, queryTypes q11]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7, Queryable q8 o8, Queryable q9 o9, Queryable q10 o10, Queryable q11 o11, Queryable q12 o12) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12) (o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity
    r8 <- runQueryEntity q8 world entity
    r9 <- runQueryEntity q9 world entity
    r10 <- runQueryEntity q10 world entity
    r11 <- runQueryEntity q11 world entity
    r12 <- runQueryEntity q12 world entity

    return $ (,,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world
    r8 <- runQueryInternal q8 archetypes world
    r9 <- runQueryInternal q9 archetypes world
    r10 <- runQueryInternal q10 archetypes world
    r11 <- runQueryInternal q11 archetypes world
    r12 <- runQueryInternal q12 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11), (_, r12)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12))) $ getZipList $ (,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8, queryTypes q9, queryTypes q10, queryTypes q11, queryTypes q12]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7, Queryable q8 o8, Queryable q9 o9, Queryable q10 o10, Queryable q11 o11, Queryable q12 o12, Queryable q13 o13) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13) (o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity
    r8 <- runQueryEntity q8 world entity
    r9 <- runQueryEntity q9 world entity
    r10 <- runQueryEntity q10 world entity
    r11 <- runQueryEntity q11 world entity
    r12 <- runQueryEntity q12 world entity
    r13 <- runQueryEntity q13 world entity

    return $ (,,,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12 <*> r13

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world
    r8 <- runQueryInternal q8 archetypes world
    r9 <- runQueryInternal q9 archetypes world
    r10 <- runQueryInternal q10 archetypes world
    r11 <- runQueryInternal q11 archetypes world
    r12 <- runQueryInternal q12 archetypes world
    r13 <- runQueryInternal q13 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11), (_, r12), (_, r13)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13))) $ getZipList $ (,,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12 <*> ZipList r13

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8, queryTypes q9, queryTypes q10, queryTypes q11, queryTypes q12, queryTypes q13]

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1, Queryable q2 o2, Queryable q3 o3, Queryable q4 o4, Queryable q5 o5, Queryable q6 o6, Queryable q7 o7, Queryable q8 o8, Queryable q9 o9, Queryable q10 o10, Queryable q11 o11, Queryable q12 o12, Queryable q13 o13, Queryable q14 o14) => Queryable (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14) (o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14) where
  runQueryEntity (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity
    r2 <- runQueryEntity q2 world entity
    r3 <- runQueryEntity q3 world entity
    r4 <- runQueryEntity q4 world entity
    r5 <- runQueryEntity q5 world entity
    r6 <- runQueryEntity q6 world entity
    r7 <- runQueryEntity q7 world entity
    r8 <- runQueryEntity q8 world entity
    r9 <- runQueryEntity q9 world entity
    r10 <- runQueryEntity q10 world entity
    r11 <- runQueryEntity q11 world entity
    r12 <- runQueryEntity q12 world entity
    r13 <- runQueryEntity q13 world entity
    r14 <- runQueryEntity q14 world entity

    return $ (,,,,,,,,,,,,,,) <$> r0 <*> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12 <*> r13 <*> r14

  runQueryInternal (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world
    r2 <- runQueryInternal q2 archetypes world
    r3 <- runQueryInternal q3 archetypes world
    r4 <- runQueryInternal q4 archetypes world
    r5 <- runQueryInternal q5 archetypes world
    r6 <- runQueryInternal q6 archetypes world
    r7 <- runQueryInternal q7 archetypes world
    r8 <- runQueryInternal q8 archetypes world
    r9 <- runQueryInternal q9 archetypes world
    r10 <- runQueryInternal q10 archetypes world
    r11 <- runQueryInternal q11 archetypes world
    r12 <- runQueryInternal q12 archetypes world
    r13 <- runQueryInternal q13 archetypes world
    r14 <- runQueryInternal q14 archetypes world

    return $ map (\((e0, r0), (_, r1), (_, r2), (_, r3), (_, r4), (_, r5), (_, r6), (_, r7), (_, r8), (_, r9), (_, r10), (_, r11), (_, r12), (_, r13), (_, r14)) -> (e0, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14))) $ getZipList $ (,,,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12 <*> ZipList r13 <*> ZipList r14

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8, queryTypes q9, queryTypes q10, queryTypes q11, queryTypes q12, queryTypes q13, queryTypes q14]
