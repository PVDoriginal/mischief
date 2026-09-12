{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.World.Insert where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.Data
import Data.Foldable (for_)
import Data.IORef
import Data.List hiding (insert)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Base (Int (..))
import GHC.Stack
import Mischief.ECS.Archetypes
import Mischief.ECS.Archetypes.Graph
  ( getArchetypeOnInsert,
  )
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.HooksDef (HookContext (..), HookContextRel (..))
import Mischief.ECS.Components.Spawn (ComponentAddHooks (ComponentAddHooks), ComponentAddHooksRel (ComponentAddHooksRel), ComponentSetHooks (ComponentSetHooks), ComponentSetHooksRel (ComponentSetHooksRel), meta)
import Mischief.ECS.Entities
import Mischief.ECS.EntityDef
import Mischief.ECS.EventDef
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.Vec qualified as Vec
import Mischief.ECS.World
import Mischief.ECS.World.Change
import Mischief.ECS.World.Prefs
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Utils

data Exception' = Exception' deriving (Show)

instance Exception Exception'

-- | Insert a bundle of components on an Entity.
--
-- If the entity already contains these components, their values will be
-- updated in-place instead of causing an archetype change.
insert :: (HasCallStack) => forall b. (Bundle b) => b -> Entity -> System ()
insert bundle entity =
  do
    world <- unsafeGetWorld
    pointer <- liftIO $ getPointer entity world.entities

    case pointer of
      Nothing -> warn $ "Insertion failed: Entity " <> text entity <> " is not alive."
      Just currentPointer -> do
        let BundleData {elements, resources, external} = bundleData bundle

        for_ external $ \(e, x) -> do
          insert x e

        for_ resources $ \BundleElement {component = ErasedComponent (val :: c)} -> do
          m <- meta @c
          insert val m

        currentTick <- liftIO $ readIORef world.tick

        bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements
        let newComponents = sort $ map (\x -> x.id) bundleData.elements

        (EntityPointer (# archetypeId, rowIndex #)) <- liftIO $ readIORef currentPointer

        currentTable <- Vec.read world.tables.inner (I# archetypeId)

        ChangeResult {requiredComponentsAdded, newComponents} <-
          if newComponents `isSubsequenceOf` currentTable.components
            -- Simple case, no archetype change.
            then do
              liftIO $ replaceComponentsIntoTable bundleData (Just currentTick) (EntityPointer (# archetypeId, rowIndex #)) currentTable
              pure $ ChangeResult [] []
            -- Complex case, archetype change.
            else do
              newArchetype <- getArchetypeOnInsert (ArchetypeId $ I# archetypeId) newComponents
              changeArchetype entity newArchetype (Just bundleData)

        unless world.prefs.supressEvents $ do
          triggerAddEvent (ProcessedBundleData newComponents) entity
          triggerAddEvent (ProcessedBundleData requiredComponentsAdded) entity
          triggerSetEvent bundleData entity
          triggerSetEvent (ProcessedBundleData requiredComponentsAdded) entity

-- getOrInsert :: forall qd. (Updateable (Result qd), Bundle qd) => qd -> Entity -> System (Result qd)
-- getOrInsert val entity = do
--   g <- update (Result (val, entity))
--   case g of
--     Just g -> return g
--     Nothing -> do
--       insert val entity
--       return $ Result (val, entity)

-- | Insert a bundle of components on an Entity.
--
-- Only the components that the entity doesn't already have will be inserted, and the rest ignored.
insertNew :: forall b. (Bundle b) => b -> Entity -> System ()
insertNew bundle entity =
  do
    let BundleData {elements, resources, external} = bundleData bundle
    for_ external $ \(e, s) -> do
      insertNew s e

    world <- unsafeGetWorld

    pointer <- liftIO $ getPointer entity world.entities

    case pointer of
      Nothing -> warn $ "Insertion failed: Entity " <> text entity <> " is not alive."
      Just currentPointer -> do
        for_ resources $ \BundleElement {component = ErasedComponent (val :: c)} -> do
          m <- meta @c
          r <- get m $ mkQuery (C @c)
          case r of
            Just _ -> pure ()
            Nothing -> insert val m

        currentTick <- liftIO $ readIORef world.tick

        bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

        (EntityPointer (# archetypeId, _ #)) <- liftIO $ readIORef currentPointer

        currentTable <- Vec.read world.tables.inner (I# archetypeId)

        let newComponents = ProcessedBundleData $ filter (\c -> c.id `notElem` currentTable.components) bundleData.elements

        unless (null newComponents.elements) $ do
          newArchetype <- getArchetypeOnInsert (ArchetypeId $ I# archetypeId) $ map (\x -> x.id) newComponents.elements
          ChangeResult {requiredComponentsAdded} <- changeArchetype entity newArchetype (Just bundleData)

          unless world.prefs.supressEvents $ do
            triggerAddEvent (ProcessedBundleData requiredComponentsAdded) entity
            triggerAddEvent newComponents entity
            triggerSetEvent (ProcessedBundleData requiredComponentsAdded) entity
            triggerSetEvent newComponents entity

insertIfNeq :: forall b. (BundleEq b) => b -> Entity -> System ()
insertIfNeq b entity = do
  let BundleData {elements, resources, external} = bundleDataEq b

  comps <- flip filterM (Set.toList elements) $ \BundleElement {rep, component = ErasedComponentEq (val :: c)} -> do
    val' <- case rep of
      PairRep (_, target) -> fmap (\x -> x.comp) <$> get entity (mkQuery (R @c target))
      _ -> get entity (mkQuery (C @c))

    case val' of
      Nothing -> return True
      Just val' -> return (val /= val')

  resources <- flip filterM (Set.toList resources) $ \BundleElement {rep, component = ErasedComponentEq (val :: c)} -> do
    e <- meta @c
    val' <- case rep of
      ComponentRep _ -> get e $ mkQuery (C @c)
      _ -> undefined

    case val' of
      Nothing -> return True
      Just val' -> return (val /= val')

  for_ external $ \(e, external) -> do
    insertIfNeq external e
    undefined

  insert (bundleEqToSimple $ BundleData (Set.fromList comps) (Set.fromList resources) Set.empty) entity

-- class Settable c i | c -> i where
--   setInner :: c -> i -> System ()

--   setIfNeqInner :: (Eq i) => c -> i -> System ()

-- class Settable' isRel c i | isRel c -> i where
--   setInner' :: c -> i -> System ()
--   setIfNeqInner' :: (Eq i) => c -> i -> System ()

-- set :: (Settable c i) => c -> i -> System ()
-- set = setInner

-- setIfNeq :: (Eq i, Settable c i) => c -> i -> System ()
-- setIfNeq = setIfNeqInner

triggerAddEvent :: ProcessedBundleData -> Entity -> System ()
triggerAddEvent bundle entity =
  for_ bundle.elements $ \x -> do
    let !(ComponentId (# _, target #)) = x.id
    case target of
      Nothing ->
        triggerAddEventC x.component.value entity
      Just target ->
        triggerAddEventR x.component.value target entity

triggerAddEventC :: ErasedComponent -> Entity -> System ()
triggerAddEventC (ErasedComponent (_ :: c)) entity = do
  let context = HookContext {entity}

  m <- meta @c
  hooks <- get m $ mkQuery (C @ComponentAddHooks)
  for_ hooks $ \(ComponentAddHooks h) -> do
    for_ h $ \h -> h context

  runEvent $ eraseEvent $ OnAdd @c entity

triggerAddEventR :: ErasedComponent -> Entity -> Entity -> System ()
triggerAddEventR (ErasedComponent (_ :: c)) target entity = do
  let context = HookContextRel {entity, target}

  m <- meta @c
  Just hooks <- get m $ mkQuery (M @ComponentAddHooksRel)
  for_ hooks $ \(ComponentAddHooksRel h) -> do
    for_ h $ \h -> h context

  runEvent $ eraseEvent $ OnAddRel @c entity target

triggerSetEvent :: ProcessedBundleData -> Entity -> System ()
triggerSetEvent bundle entity =
  for_ bundle.elements $ \x -> do
    let !(ComponentId (# _, target #)) = x.id
    case target of
      Nothing ->
        triggerSetEventC x.component.value entity
      Just target ->
        triggerSetEventR x.component.value target entity

triggerSetEventC :: ErasedComponent -> Entity -> System ()
triggerSetEventC (ErasedComponent (_ :: c)) entity = do
  let context = HookContext {entity}

  m <- meta @c
  Just hooks <- get m $ mkQuery (M @ComponentSetHooks)
  for_ hooks $ \(ComponentSetHooks h) -> do
    for_ h $ \h -> h context

  runEvent $ eraseEvent $ OnSet @c entity

triggerSetEventR :: ErasedComponent -> Entity -> Entity -> System ()
triggerSetEventR (ErasedComponent (_ :: c)) target entity = do
  let context = HookContextRel {entity, target}

  m <- meta @c
  Just hooks <- get m $ mkQuery (M @ComponentSetHooksRel)
  for_ hooks $ \(ComponentSetHooksRel h) -> do
    for_ h $ \h -> h context

  runEvent $ eraseEvent $ OnSetRel @c entity target