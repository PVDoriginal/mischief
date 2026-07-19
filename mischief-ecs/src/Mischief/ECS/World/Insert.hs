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
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Stack
import Mischief.ECS.Archetypes
import {-# SOURCE #-} Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import {-# SOURCE #-} Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Change
import Mischief.ECS.World.Prefs
import Mischief.ECS.World.Query
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
        let BundleData {elements} = bundleData bundle

        currentTick <- liftIO $ readIORef world.tick

        bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements
        let newComponents = sort $ map (\x -> x.id) bundleData.elements

        currentPointerInternal <- liftIO $ readIORef currentPointer

        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables

        case Map.lookup currentPointerInternal.archetypeId tables of
          Nothing -> undefined
          Just currentTable -> do
            -- Simple case, no archetype change.
            if newComponents `isSubsequenceOf` currentTable.components
              then
                liftIO $ replaceComponentsIntoTable bundleData (Just currentTick) currentPointerInternal currentTable
              -- Complex case, archetype change.
              else do
                newArchetype <- getArchetypeOnInsert currentPointerInternal.archetypeId newComponents
                ChangeResult {requiredComponentsAdded} <- changeArchetype entity newArchetype (Just bundleData)

                unless world.prefs.supressEvents $
                  triggerInsertEvent (ProcessedBundleData requiredComponentsAdded) entity

        unless world.prefs.supressEvents $
          triggerInsertEvent bundleData entity

getOrInsert :: forall qd. (Queryable qd (Result qd), Bundle qd) => qd -> Entity -> System (Result qd)
getOrInsert val entity = do
  g <- get val entity
  case g of
    Just g -> return g
    Nothing -> do
      insert val entity
      return $ Result (val, entity)

-- | Insert a bundle of components on an Entity.
--
-- Only the components that the entity doesn't already have will be inserted, and the rest ignored.
insertNew :: forall b. (Bundle b) => b -> Entity -> System ()
insertNew bundle entity =
  do
    world <- unsafeGetWorld
    pointer <- liftIO $ getPointer entity world.entities

    case pointer of
      Nothing -> warn $ "Insertion failed: Entity " <> text entity <> " is not alive."
      Just currentPointer -> do
        let BundleData {elements} = bundleData bundle

        currentTick <- liftIO $ readIORef world.tick

        bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

        currentPointerInternal <- liftIO $ readIORef currentPointer

        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables

        case Map.lookup currentPointerInternal.archetypeId tables of
          Nothing -> undefined
          Just currentTable -> do
            let newComponents = ProcessedBundleData $ filter (\c -> c.id `notElem` currentTable.components) bundleData.elements

            unless (null newComponents.elements) $ do
              newArchetype <- getArchetypeOnInsert currentPointerInternal.archetypeId $ map (\x -> x.id) newComponents.elements
              ChangeResult {requiredComponentsAdded} <- changeArchetype entity newArchetype (Just bundleData)

              unless world.prefs.supressEvents $
                triggerInsertEvent (ProcessedBundleData requiredComponentsAdded) entity

            unless world.prefs.supressEvents $
              triggerInsertEvent newComponents entity

insertIfNeq :: (BundleEq b) => b -> Entity -> System ()
insertIfNeq b entity = do
  let BundleData {elements} = bundleDataEq b

  comps <- flip filterM (Set.toList elements) $ \BundleElement {rep, component = ErasedComponentEq (val :: c)} -> do
    val' <- case rep of
      PairRep (_, target) -> fmap (\x -> x.comp) <$> get (R @c target) entity
      _ -> fmap value <$> get (C @c) entity

    case val' of
      Nothing -> return True
      Just val' -> return (val /= val')

  insert (bundleEqToSimple $ BundleData (Set.fromList comps)) entity

-- | Insert a resource into this world. If the resource already exists, its value will be overwritten.
insertRes :: forall r. (Component r, Bundle r) => r -> System ()
insertRes res = do
  entity <- meta @r
  insert res entity

class Settable c i | c -> i where
  setInner :: c -> i -> System ()

  setIfNeqInner :: (Eq i) => c -> i -> System ()

class Settable' isRel c i | isRel c -> i where
  setInner' :: c -> i -> System ()
  setIfNeqInner' :: (Eq i) => c -> i -> System ()

instance (Component c) => Settable' False (Result (Rel c)) c where
  setInner' :: Result (Rel c) -> c -> System ()
  setInner' !result !newValue = Mischief.ECS.World.Insert.insert (Rel newValue result.target) (entityOf result)

  setIfNeqInner' :: (Component c, Eq c) => Result (Rel c) -> c -> System ()
  setIfNeqInner' !result !newValue = do
    curr <- get (R @c result.target) (entityOf result)
    case curr of
      Nothing -> warn $ "SetIfNeq failed: Entity " <> text (entityOf result) <> " is not alive."
      Just curr ->
        when (curr.comp /= newValue) $
          setInner' @False result newValue

instance (Component c, IsComponentC c ~ HTrue) => Settable' True (Result c) c where
  setInner' :: Result c -> c -> System ()
  setInner' !result !newValue = Mischief.ECS.World.Insert.insert newValue (entityOf result)

  setIfNeqInner' :: (Component c, Eq c) => Result c -> c -> System ()
  setIfNeqInner' !result !newValue = do
    curr <- get (C @c) (entityOf result)
    case curr of
      Nothing -> warn $ "SetIfNeq failed: Entity " <> text (entityOf result) <> " is not alive."
      Just curr ->
        when (value curr /= newValue) $
          setInner' @True result newValue

instance (Settable' (IsComp c) (Result c) i) => Settable (Result c) i where
  setInner = setInner' @(IsComp c)
  setIfNeqInner = setIfNeqInner' @(IsComp c)

-- | Set the value of a component obtained as query result.
--
-- Note that the local 'Result' won't be mutated.
-- You'll need to query the component again or use 'update' to update the current result.
set :: (Settable c i) => c -> i -> System ()
set = setInner

setIfNeq :: (Eq i, Settable c i) => c -> i -> System ()
setIfNeq = setIfNeqInner

class Updateable' flag r where
  updateInner' :: r -> System (Maybe r)

instance (Component c, IsComponentC c ~ HTrue) => Updateable' True (Result c) where
  updateInner' r = get (C @c) (entityOf r)

instance (Component c) => Updateable' False (Result (Rel c)) where
  updateInner' r = get (R @c r.target) (entityOf r)

class Updateable r where
  updateInner :: r -> System (Maybe r)

instance (Updateable' (IsComp c) (Result c)) => Updateable (Result c) where
  updateInner = updateInner' @(IsComp c)

-- | Update the value of a 'Result'.
--
-- Useful if you've done changed to the component and want to grab the live value
-- without re-querying.
update :: forall c. (Updateable (Result c)) => Result c -> System (Maybe (Result c))
update = updateInner

triggerInsertEvent :: ProcessedBundleData -> Entity -> System ()
triggerInsertEvent bundle entity =
  for_ bundle.elements $ \x -> do
    case x.id.entity of
      Nothing ->
        triggerInsertEventC x.component.value entity
      Just target ->
        triggerInsertEventR x.component.value target entity

triggerInsertEventC :: ErasedComponent -> Entity -> System ()
triggerInsertEventC (ErasedComponent (_ :: c)) entity =
  runEvent $ eraseEvent $ OnInsert @c entity

triggerInsertEventR :: ErasedComponent -> Entity -> Entity -> System ()
triggerInsertEventR (ErasedComponent (_ :: c)) target entity = do
  runEvent $ eraseEvent $ OnInsertRel @c entity target