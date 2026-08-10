{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Mischief.ECS.World.Query.Queryable where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Bifunctor qualified
import Data.Data
import Data.IORef
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import GHC.Base (Int (I#), eqWord#, isTrue#)
import Mischief.ECS.App.SystemDef
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Mappable
import Mischief.ECS.Tables
import Mischief.ECS.Vec qualified as Vec
import Mischief.ECS.World
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Utils

data TypeQuery = CompQ | RelQ | RelQ' Entity deriving (Eq, Ord, Show)

data RelTarget = AnyTarget | RelTargets [Entities]

filterQueryIO :: QueryFilter -> World -> Entity -> IO Bool
filterQueryIO a w e = runSystem (filterQuery a e) w

class Queryable qd output | qd -> output where
  runQueryEntity :: qd -> World -> Entity -> IO (Maybe output)
  runQueryInternal :: qd -> [ArchetypeId] -> World -> IO [(Entity, Bool, output)]
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

  runQueryInternal _ archetypes world = map (\(a, b) -> (a, True, b)) <$> tryGetComponents @c world archetypes

  queryTypes _ = Set.singleton (typeRep (Proxy @c), CompQ)

instance Queryable E Entity where
  runQueryEntity _ _ entity = return $ Just entity

  runQueryInternal _ archetypes world = map (\x -> (x, True, x)) <$> tryGetEntities world archetypes

  queryTypes _ = Set.empty

class RelQuery (exclusive :: Exclusivity) qd output | qd exclusive -> output where
  relRunQueryEntity :: qd -> World -> Entity -> IO (Maybe output)
  relRunQueryInternal :: qd -> [ArchetypeId] -> World -> IO [(Entity, Bool, output)]
  relQueryTypes :: qd -> Set (TypeRep, TypeQuery)

instance (Component c) => RelQuery Inclusive (R c Any) [Result (Rel c)] where
  relRunQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Just (Just x) -> Just x
      _ -> Nothing

  relRunQueryInternal _ archetypes world = map (\(a, b) -> (a, True, b)) <$> tryGetRelCollections @c world archetypes

  relQueryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance (Component c) => RelQuery Exclusive (R c Any) (Result (Rel c)) where
  relRunQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    return $ case res of
      Just (Just [x]) -> Just x
      _ -> Nothing

  relRunQueryInternal _ archetypes world = do
    rels <- tryGetRelCollections @c world archetypes
    return $ map (\(e, x : _) -> (e, True, x)) rels

  relQueryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance (Component c, Queryable q out) => RelQuery Inclusive (R c (Q q)) [out] where
  relRunQueryEntity (R (Q q)) world entity = do
    res <- relRunQueryEntity @Inclusive (R @c Any) world entity

    case fmap (traverse $ \r -> runQueryEntity q world r.target) res of
      Nothing -> pure Nothing
      Just x -> do
        x <- x
        pure $ case catMaybes x of
          [] -> Nothing
          x -> Just x

  relRunQueryInternal (R (Q q)) archetypes world = do
    res <- relRunQueryInternal @Inclusive (R @c Any) archetypes world
    for res $ \(e, b, rels) -> do
      a <- catMaybes <$> (traverse $ \r -> runQueryEntity q world r.target) rels

      pure $ case a of
        [] -> (e, False, undefined)
        x -> (e, b, x)

  relQueryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance (Component c, Queryable q out, Collectable f QueryFilter) => RelQuery Inclusive (R c (Q' q f)) [out] where
  relRunQueryEntity (R (Q' q f')) world entity = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryEntity @Inclusive (R @c Any) world entity

    case fmap
      ( traverse $ \r -> do
          t <- filterQueryIO f world r.target
          if t
            then
              runQueryEntity q world r.target
            else pure Nothing
      )
      res of
      Nothing -> pure Nothing
      Just x -> do
        x <- x
        pure $ case catMaybes x of
          [] -> Nothing
          x -> Just x

  relRunQueryInternal (R (Q' q f')) archetypes world = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryInternal @Inclusive (R @c Any) archetypes world
    for res $ \(e, b, rels) -> do
      a <-
        catMaybes
          <$> ( traverse $ \r -> do
                  t <- filterQueryIO f world r.target
                  if t
                    then
                      runQueryEntity q world r.target
                    else pure Nothing
              )
            rels

      pure $ case a of
        [] -> (e, False, undefined)
        x -> (e, b, x)

  relQueryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance (Component c, Queryable q out) => RelQuery Exclusive (R c (Q q)) out where
  relRunQueryEntity (R (Q q)) world entity = do
    res <- relRunQueryEntity @Exclusive (R @c Any) world entity

    case fmap (\r -> runQueryEntity q world r.target) res of
      Nothing -> pure Nothing
      Just x -> x
  relRunQueryInternal (R (Q q)) archetypes world = do
    res <- relRunQueryInternal @Exclusive (R @c Any) archetypes world
    for res $ \(e, b, rels) -> do
      r <- (\r -> runQueryEntity q world r.target) rels
      pure $ case r of
        Nothing -> (e, False, undefined)
        Just x -> (e, b, x)

  relQueryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance (Component c, Queryable q out, Collectable f QueryFilter) => RelQuery Exclusive (R c (Q' q f)) out where
  relRunQueryEntity (R (Q' q f')) world entity = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryEntity @Exclusive (R @c Any) world entity

    case fmap
      ( \r -> do
          t <- filterQueryIO f world r.target
          if t
            then
              runQueryEntity q world r.target
            else pure Nothing
      )
      res of
      Nothing -> pure Nothing
      Just x -> x
  relRunQueryInternal (R (Q' q f')) archetypes world = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryInternal @Exclusive (R @c Any) archetypes world
    for res $ \(e, b, rels) -> do
      r <-
        ( \r -> do
            t <- filterQueryIO f world r.target
            if t
              then
                runQueryEntity q world r.target
              else pure Nothing
        )
          rels
      pure $ case r of
        Nothing -> (e, False, undefined)
        Just x -> (e, b, x)

  relQueryTypes _ = Set.singleton (typeRep $ Proxy @c, RelQ)

instance (Component c) => Queryable (R c Entity) (Result (Rel c)) where
  runQueryEntity (R target) world entity = do
    res <- tryGetEntityRel @c target world entity
    return $ case res of
      Just (Just x) -> Just $ Result (Rel x target, entity)
      _ -> Nothing

  runQueryInternal (R target) archetypes world = map (\(a, b) -> (a, True, b)) <$> tryGetRels @c target world archetypes

  queryTypes (R target) = Set.singleton (typeRep $ Proxy @c, RelQ' target)

instance (RelQuery (RelExclusivity c) (R c Any) out) => Queryable (R c Any) out where
  runQueryEntity = relRunQueryEntity @(RelExclusivity c)

  runQueryInternal = relRunQueryInternal @(RelExclusivity c)

  queryTypes = relQueryTypes @(RelExclusivity c)

instance (RelQuery (RelExclusivity c) (R c (Q q)) out) => Queryable (R c (Q q)) out where
  runQueryEntity = relRunQueryEntity @(RelExclusivity c)

  runQueryInternal = relRunQueryInternal @(RelExclusivity c)

  queryTypes = relQueryTypes @(RelExclusivity c)

instance (RelQuery (RelExclusivity c) (R c (Q' q f)) out) => Queryable (R c (Q' q f)) out where
  runQueryEntity = relRunQueryEntity @(RelExclusivity c)

  runQueryInternal = relRunQueryInternal @(RelExclusivity c)

  queryTypes = relQueryTypes @(RelExclusivity c)

instance (RelQuery Inclusive (R c e) out) => Queryable (R' c e) out where
  runQueryEntity (R' e) = relRunQueryEntity @Inclusive (R @c e)
  runQueryInternal (R' e) = relRunQueryInternal @Inclusive (R @c e)
  queryTypes (R' e) = relQueryTypes @Inclusive (R @c e)

instance (Component c) => Queryable (M c) (Maybe (Result c)) where
  runQueryEntity _ world entity = do
    res <- tryGetEntityComponent @c world entity
    case res of
      Nothing -> return Nothing
      Just Nothing -> return $ Just Nothing
      Just (Just x) -> return $ Just $ Just $ Result (x, entity)

  runQueryInternal _ archetypes world = map (\(a, b) -> (a, True, b)) <$> tryGetComponentsMaybe @c world archetypes

  queryTypes _ = Set.empty

instance (Component c) => Queryable (MR c Entity) (Maybe (Result (Rel c))) where
  runQueryEntity (MR target) world entity = do
    res <- tryGetEntityRel @c target world entity
    return $ case res of
      Nothing -> Nothing
      Just Nothing -> Just Nothing
      Just (Just x) -> Just $ Just (Result (Rel x target, entity))

  runQueryInternal (MR target) archetypes world = map (\(a, b) -> (a, True, b)) <$> tryGetRelsMaybe @c target world archetypes
  queryTypes _ = Set.empty

instance (Component c) => RelQuery Inclusive (MR c Any) (Maybe [Result (Rel c)]) where
  relRunQueryEntity _ = tryGetEntityRelCollection @c

  relRunQueryInternal _ archetypes world = do
    x <- tryGetRelCollections @c world archetypes
    return $ flip map x $ \(e, x) ->
      case x of
        [] -> (e, True, Nothing)
        x -> (e, True, Just x)
  relQueryTypes _ = Set.empty

instance (Component c) => RelQuery Exclusive (MR c Any) (Maybe (Result (Rel c))) where
  relRunQueryEntity _ world entity = do
    res <- tryGetEntityRelCollection @c world entity
    pure $ case res of
      Just (Just [x]) -> Just (Just x)
      Just (Just _) -> undefined
      Just Nothing -> Just Nothing
      Nothing -> Nothing

  relRunQueryInternal _ archetypes world = do
    x <- tryGetRelCollections @c world archetypes
    return $ flip map x $ \(e, x) ->
      case x of
        [] -> (e, True, Nothing)
        [x] -> (e, True, Just x)
        _ -> undefined
  relQueryTypes _ = Set.empty

instance (Component c, Queryable q out) => RelQuery Inclusive (MR c (Q q)) (Maybe [out]) where
  relRunQueryEntity (MR (Q q)) world entity = do
    res <- relRunQueryEntity @Inclusive (MR @c Any) world entity
    case res of
      Nothing -> pure Nothing
      Just Nothing -> pure $ Just Nothing
      Just (Just r) ->
        Just <$> do
          let targets = map (\x -> x.target) r
          results <- catMaybes <$> for targets (runQueryEntity q world)
          case results of
            [] -> pure Nothing
            x -> pure $ Just x

  relRunQueryInternal (MR (Q q)) archetypes world = do
    res <- relRunQueryInternal @Inclusive (MR @c Any) archetypes world
    for res $ \(e, b, r) -> do
      case r of
        Nothing -> pure (e, b, Nothing)
        Just r -> do
          rs <- for r (\x -> runQueryEntity q world x.target)
          case catMaybes rs of
            [] -> pure (e, b, Nothing)
            x -> pure (e, b, Just x)

  relQueryTypes _ = Set.empty

instance (Component c, Queryable q out, Collectable f QueryFilter) => RelQuery Inclusive (MR c (Q' q f)) (Maybe [out]) where
  relRunQueryEntity (MR (Q' q f')) world entity = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryEntity @Inclusive (MR @c Any) world entity
    case res of
      Nothing -> pure Nothing
      Just Nothing -> pure $ Just Nothing
      Just (Just r) ->
        Just <$> do
          let targets = map (\x -> x.target) r
          results <-
            catMaybes
              <$> for
                targets
                ( \t -> do
                    b <- filterQueryIO f world t
                    if b
                      then
                        runQueryEntity q world t
                      else
                        pure Nothing
                )
          case results of
            [] -> pure Nothing
            x -> pure $ Just x

  relRunQueryInternal (MR (Q' q f')) archetypes world = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryInternal @Inclusive (MR @c Any) archetypes world
    for res $ \(e, b, r) -> do
      case r of
        Nothing -> pure (e, b, Nothing)
        Just r -> do
          rs <-
            for
              r
              ( \x -> do
                  t <- filterQueryIO f world x.target
                  if t
                    then
                      runQueryEntity q world x.target
                    else pure Nothing
              )
          case catMaybes rs of
            [] -> pure (e, b, Nothing)
            x -> pure (e, b, Just x)

  relQueryTypes _ = Set.empty

instance (Component c, Queryable q out) => RelQuery Exclusive (MR c (Q q)) (Maybe out) where
  relRunQueryEntity (MR (Q q)) world entity = do
    res <- relRunQueryEntity @Exclusive (MR @c Any) world entity
    case res of
      Nothing -> pure Nothing
      Just Nothing -> pure $ Just Nothing
      Just (Just x) -> Just <$> runQueryEntity q world x.target

  relRunQueryInternal (MR (Q q)) archetypes world = do
    res <- relRunQueryInternal @Exclusive (MR @c Any) archetypes world
    for res $ \(e, b, r) -> do
      case r of
        Nothing -> pure (e, b, Nothing)
        Just r -> do
          res <- runQueryEntity q world r.target
          pure (e, b, res)

  relQueryTypes _ = Set.empty

instance (Component c, Queryable q out, Collectable f QueryFilter) => RelQuery Exclusive (MR c (Q' q f)) (Maybe out) where
  relRunQueryEntity (MR (Q' q f')) world entity = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryEntity @Exclusive (MR @c Any) world entity
    case res of
      Nothing -> pure Nothing
      Just Nothing -> pure $ Just Nothing
      Just (Just x) -> do
        t <- filterQueryIO f world x.target
        if t
          then
            Just <$> runQueryEntity q world x.target
          else pure (Just Nothing)

  relRunQueryInternal (MR (Q' q f')) archetypes world = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryInternal @Exclusive (MR @c Any) archetypes world
    for res $ \(e, b, r) -> do
      case r of
        Nothing -> pure (e, b, Nothing)
        Just r -> do
          res <- do
            t <- filterQueryIO f world r.target
            if t
              then
                runQueryEntity q world r.target
              else pure Nothing
          pure (e, b, res)

  relQueryTypes _ = Set.empty

instance (RelQuery (RelExclusivity c) (MR c Any) out) => Queryable (MR c Any) out where
  runQueryEntity = relRunQueryEntity @(RelExclusivity c)

  runQueryInternal = relRunQueryInternal @(RelExclusivity c)
  queryTypes _ = Set.empty

instance (RelQuery (RelExclusivity c) (MR c (Q q)) out) => Queryable (MR c (Q q)) out where
  runQueryEntity = relRunQueryEntity @(RelExclusivity c)

  runQueryInternal = relRunQueryInternal @(RelExclusivity c)
  queryTypes _ = Set.empty

instance (RelQuery (RelExclusivity c) (MR c (Q' q f)) out) => Queryable (MR c (Q' q f)) out where
  runQueryEntity = relRunQueryEntity @(RelExclusivity c)

  runQueryInternal = relRunQueryInternal @(RelExclusivity c)
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
    return $ map ((\(a, b) -> (a, True, b)) . Data.Bifunctor.second null) res

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

instance (Component c, Queryable q out) => Queryable (HasR c (Q q)) Bool where
  runQueryEntity (HasR (Q q)) world entity = do
    res <- tryGetEntityRelCollection @c world entity
    case res of
      Nothing -> pure Nothing
      Just Nothing -> pure $ Just False
      Just (Just r) -> do
        res <- catMaybes <$> for r (\r -> runQueryEntity q world r.target)
        case res of
          [] -> pure $ Just False
          _ -> pure $ Just True

  runQueryInternal (HasR (Q q)) archetypes world = do
    res <- relRunQueryInternal @Inclusive (MR @c Any) archetypes world
    for res $ \(e, b, r) -> do
      case r of
        Nothing -> pure (e, b, False)
        Just r -> do
          res <- catMaybes <$> for r (\r -> runQueryEntity q world r.target)
          case res of
            [] -> pure (e, b, False)
            _ -> pure (e, b, True)

  queryTypes _ = Set.empty

instance (Component c, Queryable q out, Collectable f QueryFilter) => Queryable (HasR c (Q' q f)) Bool where
  runQueryEntity (HasR (Q' q f')) world entity = do
    let f :: QueryFilter = collect f'
    res <- tryGetEntityRelCollection @c world entity
    case res of
      Nothing -> pure Nothing
      Just Nothing -> pure $ Just False
      Just (Just r) -> do
        res <-
          catMaybes
            <$> for
              r
              ( \r -> do
                  t <- filterQueryIO f world r.target
                  if t
                    then
                      runQueryEntity q world r.target
                    else pure Nothing
              )
        case res of
          [] -> pure $ Just False
          _ -> pure $ Just True

  runQueryInternal (HasR (Q' q f')) archetypes world = do
    let f :: QueryFilter = collect f'
    res <- relRunQueryInternal @Inclusive (MR @c Any) archetypes world
    for res $ \(e, b, r) -> do
      case r of
        Nothing -> pure (e, b, False)
        Just r -> do
          res <-
            catMaybes
              <$> for
                r
                ( \r -> do
                    t <- filterQueryIO f world r.target
                    if t
                      then
                        runQueryEntity q world r.target
                      else pure Nothing
                )
          case res of
            [] -> pure (e, b, False)
            _ -> pure (e, b, True)

  queryTypes _ = Set.empty

instance (Queryable qd out, Mappable MapQueryVal out out') => Queryable (Val qd) out' where
  runQueryEntity (Val qd) b c = do
    x <- runQueryEntity qd b c
    return $ fmap (mapTuple @MapQueryVal) x
  runQueryInternal (Val qd) b c = do
    x <- runQueryInternal qd b c
    return $ map (\(a, b, c) -> (a, b, mapTuple @MapQueryVal c)) x

  queryTypes (Val qd) = queryTypes qd

filterQuery :: (MonadSystem w m) => QueryFilter -> Entity -> m Bool
filterQuery NoFilter _ = pure True
filterQuery (QFWith (x, Nothing)) entity = do
  world <- unsafeGetWorld
  comp <- liftIO $ getComponentId x world.components
  case comp of
    Nothing -> return False
    Just (ComponentId (# id, _ #)) -> do
      Just (ComponentType (_ :: Proxy a)) <- liftIO $ runQueryEntity (Val (C @ComponentType)) world (Entity (# id, 0## #))
      a <- liftIO $ runQueryEntity (Has @a) world entity
      pure $ fromMaybe False a
filterQuery (QFWith (x, Just e)) entity = do
  world <- unsafeGetWorld
  comp <- liftIO $ getComponentId x world.components
  case comp of
    Nothing -> return False
    Just (ComponentId (# id, _ #)) -> do
      Just (ComponentType (_ :: Proxy a)) <- liftIO $ runQueryEntity (Val (C @ComponentType)) world (Entity (# id, 0## #))
      a <- liftIO $ runQueryEntity (HasR @a e) world entity
      pure $ fromMaybe False a
filterQuery (QFWithRelAny x) entity = do
  world <- unsafeGetWorld
  comp <- liftIO $ getComponentId x world.components
  case comp of
    Nothing -> return False
    Just (ComponentId (# id, _ #)) -> do
      Just (ComponentType (_ :: Proxy a)) <- liftIO $ runQueryEntity (Val (C @ComponentType)) world (Entity (# id, 0## #))
      a <- liftIO $ runQueryEntity (HasR @a Any) world entity
      pure $ fromMaybe False a
filterQuery (QFChanged (x, Nothing) f) entity = do
  world <- unsafeGetWorld
  comp <- liftIO $ getComponentId x world.components
  case comp of
    Nothing -> return False
    Just comp -> do
      addedChanged' f comp entity
filterQuery (QFChanged (x, Just e) f) entity = do
  world <- unsafeGetWorld
  comp <- liftIO $ getComponentId x world.components
  case comp of
    Nothing -> return False
    Just comp -> do
      addedChanged' f (setCompIdTarget (Just e) comp) entity
filterQuery (QFChangedRelAny x f) entity = do
  world <- unsafeGetWorld
  comp <- liftIO $ getComponentId x world.components
  case comp of
    Nothing -> return False
    Just (ComponentId (# id, _ #)) -> do
      components <- liftIO $ findComponentsOfEntity world entity
      case components of
        Nothing -> return True
        Just components' -> do
          let components = filter (\(ComponentId (# id', a #)) -> isJust a && isTrue# (eqWord# id id')) components'
          and <$> mapM (\c -> addedChanged' f c entity) components
filterQuery (QFCheckRaw (_, Nothing, ErasedCheck (f :: (c -> Bool)))) entity = do
  w <- unsafeGetWorld
  a <- liftIO $ runQueryEntity (C @c) w entity
  pure $ case a of
    Nothing -> False
    Just a -> f $ value a
filterQuery (QFCheckRaw (_, Just e, ErasedCheck (f :: (c -> Bool)))) entity = do
  world <- unsafeGetWorld
  a <- liftIO $ runQueryEntity (R @c e) world entity
  pure $ case a of
    Nothing -> False
    Just a -> f a.comp
filterQuery (QFCheckRawRelAny (_, ErasedCheck (f :: (c -> Bool)))) entity = do
  world <- unsafeGetWorld
  a <- liftIO $ runQueryEntity (R' @c Any) world entity
  pure $ case a of
    Nothing -> False
    Just a -> any (\x -> f x.comp) a
filterQuery (a `QFAnd` b) entity = do
  a <- filterQuery a entity
  b <- filterQuery b entity
  pure $ a && b
filterQuery (a `QFOr` b) entity = do
  a <- filterQuery a entity
  b <- filterQuery b entity
  pure $ a || b
filterQuery (QFNot a) entity = do
  a <- filterQuery a entity
  pure $ not a

findComponentsOfEntity :: World -> Entity -> IO (Maybe [ComponentId])
findComponentsOfEntity world entity = do
  pointer <- getPointer entity world.entities

  case pointer of
    Nothing -> return Nothing
    Just x -> do
      (EntityPointer (# archetypeId, _ #)) <- readIORef x

      x <- Vec.read world.tables.inner (I# archetypeId)

      pure $ Just x.components

addedChanged' :: forall m w. (MonadSystem w m) => (ComponentTicks -> Tick -> Tick -> Bool) -> ComponentId -> Entity -> m Bool
addedChanged' f id entity = do
  world <- unsafeGetWorld
  ticks <- liftIO $ tryGetEntityTicks entity id world
  case ticks of
    Nothing -> return False
    Just ticks -> do
      (lastSystemTick, currentSystemTick) <- liftIO $ getSystemTicksInternal world
      return $ f ticks lastSystemTick currentSystemTick

getSystemTicksInternal :: World -> IO (Tick, Tick)
getSystemTicksInternal world = do
  let (SystemId sys) = world.systemId
  Just a <- runQueryEntity (C @LastSystemTick) world sys
  Just b <- runQueryEntity (C @SystemTick) world sys
  return (a.inner, b.inner)

instance {-# OVERLAPPING #-} (Queryable q0 o0, Queryable q1 o1) => Queryable (q0, q1) (o0, o1) where
  runQueryEntity (q0, q1) world entity = do
    r0 <- runQueryEntity q0 world entity
    r1 <- runQueryEntity q1 world entity

    return $ (,) <$> r0 <*> r1

  runQueryInternal (q0, q1) archetypes world = do
    r0 <- runQueryInternal q0 archetypes world
    r1 <- runQueryInternal q1 archetypes world

    return $ map (\((e0, b0, r0), (_, b1, r1)) -> (e0, b0 || b1, (r0, r1))) $ getZipList $ (,) <$> ZipList r0 <*> ZipList r1

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2)) -> (e0, b0 || b1 || b2, (r0, r1, r2))) $ getZipList $ (,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3)) -> (e0, b0 || b1 || b2 || b3, (r0, r1, r2, r3))) $ getZipList $ (,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4)) -> (e0, b0 || b1 || b2 || b3 || b4, (r0, r1, r2, r3, r4))) $ getZipList $ (,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5, (r0, r1, r2, r3, r4, r5))) $ getZipList $ (,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6, (r0, r1, r2, r3, r4, r5, r6))) $ getZipList $ (,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7, (r0, r1, r2, r3, r4, r5, r6, r7))) $ getZipList $ (,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7), (_, b8, r8)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7 || b8, (r0, r1, r2, r3, r4, r5, r6, r7, r8))) $ getZipList $ (,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7), (_, b8, r8), (_, b9, r9)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7 || b8 || b9, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9))) $ getZipList $ (,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7), (_, b8, r8), (_, b9, r9), (_, b10, r10)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7 || b8 || b9 || b10, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10))) $ getZipList $ (,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7), (_, b8, r8), (_, b9, r9), (_, b10, r10), (_, b11, r11)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7 || b8 || b9 || b10 || b11, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11))) $ getZipList $ (,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7), (_, b8, r8), (_, b9, r9), (_, b10, r10), (_, b11, r11), (_, b12, r12)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7 || b8 || b9 || b10 || b11 || b12, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12))) $ getZipList $ (,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7), (_, b8, r8), (_, b9, r9), (_, b10, r10), (_, b11, r11), (_, b12, r12), (_, b13, r13)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7 || b8 || b9 || b10 || b11 || b12 || b13, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13))) $ getZipList $ (,,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12 <*> ZipList r13

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

    return $ map (\((e0, b0, r0), (_, b1, r1), (_, b2, r2), (_, b3, r3), (_, b4, r4), (_, b5, r5), (_, b6, r6), (_, b7, r7), (_, b8, r8), (_, b9, r9), (_, b10, r10), (_, b11, r11), (_, b12, r12), (_, b13, r13), (_, b14, r14)) -> (e0, b0 || b1 || b2 || b3 || b4 || b5 || b6 || b7 || b8 || b9 || b10 || b11 || b12 || b13 || b14, (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14))) $ getZipList $ (,,,,,,,,,,,,,,) <$> ZipList r0 <*> ZipList r1 <*> ZipList r2 <*> ZipList r3 <*> ZipList r4 <*> ZipList r5 <*> ZipList r6 <*> ZipList r7 <*> ZipList r8 <*> ZipList r9 <*> ZipList r10 <*> ZipList r11 <*> ZipList r12 <*> ZipList r13 <*> ZipList r14

  queryTypes (q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14) = Set.unions [queryTypes q0, queryTypes q1, queryTypes q2, queryTypes q3, queryTypes q4, queryTypes q5, queryTypes q6, queryTypes q7, queryTypes q8, queryTypes q9, queryTypes q10, queryTypes q11, queryTypes q12, queryTypes q13, queryTypes q14]
