{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Query.Queryable where

import Control.Applicative
import Data.Bifunctor qualified
import Data.Data
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
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

data Val a = Val a

data C a = C

newtype R a b = R b

data Any = Any

data RelTarget = AnyTarget | RelTargets [Entities]

data M a = M

class Queryable qd output | qd -> output where
  runQueryEntity :: qd -> World -> Entity -> IO (Maybe output)
  runQueryInternal :: qd -> [ArchetypeId] -> World -> IO [(Entity, output)]
  queryTypes :: qd -> Set (TypeRep, TypeQuery)

instance (Component c) => Queryable (C c) (Result c) where
  runQueryEntity :: C c -> World -> Entity -> IO (Maybe (Result c))
  runQueryEntity _ world entity = do
    result <- tryGetEntityComponent @c world entity
    return $ case result of
      Just (Just res) -> Just $ Result (res, entity)
      _ -> Nothing

  runQueryInternal :: C c -> [ArchetypeId] -> World -> IO [(Entity, Result c)]
  runQueryInternal _ archetypes world = tryGetComponents @c world archetypes

  queryTypes c = Set.singleton (typeRep c, CompQ)

instance (Component c) => Queryable (R c Any) [RelResult c] where
  runQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Just (Just x) -> Just x
      _ -> Nothing

  runQueryInternal _ archetypes world = tryGetRelCollections @c world archetypes

  queryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance (Component c) => Queryable (R c Entity) (RelResult c) where
  runQueryEntity (R target) world entity = do
    res <- tryGetEntityRel @c target world entity
    return $ case res of
      Just (Just x) -> Just $ RelResult (x, entity, target)
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

instance (Component c) => Queryable (MR c Any) [RelResult c] where
  runQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just []
      Just (Just x) -> Just x

  runQueryInternal _ archetypes world = tryGetRelCollections @c world archetypes
  queryTypes _ = Set.empty

instance (Component c) => Queryable (MR c Entity) (Maybe (RelResult c)) where
  runQueryEntity (MR target) world entity = do
    res <- tryGetEntityRel @c target world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just Nothing
      Just (Just x) -> Just $ Just (RelResult (x, entity, target))

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

instance Queryable Entity Entity where
  runQueryEntity _ _ entity = pure (Just entity)
  runQueryInternal _ archetypes world = do
    results <- tryGetComponents @Entity world archetypes
    pure $ fmap (\cr -> (fst cr, fst cr)) results

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
