{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Utils where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Bifunctor qualified
import Data.IORef
import Data.List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable
import GHC.Stack
import Mischief.ECS.Archetypes
import {-# SOURCE #-} Mischief.ECS.Archetypes.Graph (getArchetypeOnRemove)
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import {-# SOURCE #-} Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import {-# SOURCE #-} Mischief.ECS.Events
import Mischief.ECS.Hidden
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.Utils
import Mischief.ECS.World
import Mischief.ECS.World.Prefs
import {-# SOURCE #-} Mischief.ECS.World.Spawn (spawnIO)

-- | Process a 'BundleElement', turning its 'TypeRep' into a 'ComponentId'.
processBundleElement :: World -> ComponentTicks -> (BundleElement ErasedComponent) -> IO ProcessedBundleElement
processBundleElement world ticks BundleElement {rep = (ComponentRep r), component} =
  do
    id <- runSystem (getOrAddComponentId r) world
    return
      ProcessedBundleElement
        { id,
          component =
            ComponentData
              { value = component,
                ticks
              }
        }
processBundleElement world ticks BundleElement {rep = (PairRep (r, entity)), component} =
  do
    id <- runSystem (getOrAddPairId (Pair (r, entity))) world
    return
      ProcessedBundleElement
        { id,
          component =
            ComponentData
              { value = component,
                ticks
              }
        }

-- | Process a set of 'BundleElement's into a 'ProcessedBundleData'.
processBundleElements :: World -> ComponentTicks -> Set (BundleElement ErasedComponent) -> IO ProcessedBundleData
processBundleElements world ticks elements =
  do
    elements <- mapM (processBundleElement world ticks) (Set.toList elements)
    return
      ProcessedBundleData {elements}

-- | Combine two 'ProcessedBundleData's, merging their sets of elements.
combineProcessedBundles :: ProcessedBundleData -> ProcessedBundleData -> ProcessedBundleData
combineProcessedBundles bundle1 bundle2 =
  let elements = Set.toList $ Set.union (Set.fromList bundle1.elements) (Set.fromList bundle2.elements)
   in ProcessedBundleData {elements}

-- | Check if a 'ComponentId' is inside a 'ProcessedBundleData'.
isInProcessedBundle :: ProcessedBundleData -> ComponentId -> Bool
isInProcessedBundle ProcessedBundleData {elements} id = id `elem` map (\element -> element.id) elements

-- | Sets the change tick of certain elements of the bundle to the specified 'Tick'.
setChangedTickOfComponents :: ProcessedBundleData -> (ComponentId -> Bool) -> Tick -> ProcessedBundleData
setChangedTickOfComponents ProcessedBundleData {elements} shouldChange tick =
  ProcessedBundleData
    { elements =
        map
          ( \ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}} ->
              if shouldChange id
                then ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed = tick}}}
                else ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}}
          )
          elements
    }

-- | Sets the added tick of certain elements of the bundle to the specified 'Tick'.
setAddedTickOfComponents :: ProcessedBundleData -> (ComponentId -> Bool) -> Tick -> ProcessedBundleData
setAddedTickOfComponents ProcessedBundleData {elements} shouldChange tick =
  ProcessedBundleData
    { elements =
        map
          ( \ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}} ->
              if shouldChange id
                then ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added = tick, changed}}}
                else ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}}
          )
          elements
    }

removeComponentFromProcessedBundle :: ComponentId -> ProcessedBundleData -> ProcessedBundleData
removeComponentFromProcessedBundle componentId bundle =
  do
    let elements = filter (\x -> x.id /= componentId) bundle.elements
     in ProcessedBundleData {elements}

tryGetEntityRelCollection :: forall c. (Component c) => World -> Entity -> IO (Maybe (Maybe [Result (Rel c)]))
tryGetEntityRelCollection world entity =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return Nothing
      Just componentId -> do
        pointer <- getPointer entity world.entities
        case pointer of
          Nothing -> do
            return Nothing
          Just pointer ->
            do
              pointer <- readIORef pointer
              res <- tryGetRelCollectionFromTables world.tables entity pointer componentId
              return $ Just res

tryGetEntityComponent :: forall c. (Component c) => World -> Entity -> IO (Maybe (Maybe c))
tryGetEntityComponent world entity =
  do
    pointer <- getPointer entity world.entities

    case pointer of
      Nothing -> return Nothing
      Just pointer ->
        do
          componentId <- getComponentId (typeRep $ Proxy @c) world.components
          case componentId of
            Nothing -> return Nothing
            Just componentId -> do
              pointer <- readIORef pointer
              res <- tryGetComponentFromTables world.tables pointer componentId
              return $ Just res

tryGetEntityRel :: forall c. (Component c) => Entity -> World -> Entity -> IO (Maybe (Maybe c))
tryGetEntityRel target world entity =
  do
    pointer <- getPointer entity world.entities

    case pointer of
      Nothing -> return Nothing
      Just pointer ->
        do
          componentId <- getComponentId (typeRep $ Proxy @c) world.components
          case componentId of
            Nothing -> return Nothing
            Just componentId -> do
              pointer <- readIORef pointer
              res <- tryGetComponentFromTables world.tables pointer componentId {entity = Just target}
              return $ Just res

tryGetRelCollections :: forall c. (Component c) => World -> [ArchetypeId] -> IO [(Entity, [Result (Rel c)])]
tryGetRelCollections world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return []
      Just componentId ->
        tryGetRelCollectionsFromTables world.tables archetypes componentId

tryGetComponents :: forall c. (Component c) => World -> [ArchetypeId] -> IO [(Entity, Result c)]
tryGetComponents world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return []
      Just componentId ->
        tryGetComponentsFromTables world.tables archetypes componentId

tryGetEntities :: World -> [ArchetypeId] -> IO [Entity]
tryGetEntities world = tryGetEntitiesFromTables world.tables

tryGetRels :: forall c. (Component c) => Entity -> World -> [ArchetypeId] -> IO [(Entity, Result (Rel c))]
tryGetRels target world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return []
      Just componentId -> do
        res <- tryGetComponentsFromTables world.tables archetypes componentId {entity = Just target}
        return $ map (\(e, res) -> (e, Result (Rel (value res) target, entityOf res))) res

tryGetRelsMaybe :: forall c. (Component c) => Entity -> World -> [ArchetypeId] -> IO [(Entity, Maybe (Result (Rel c)))]
tryGetRelsMaybe target world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return []
      Just componentId -> do
        res <- tryGetComponentsFromTablesMaybe world.tables archetypes componentId {entity = Just target}
        return $
          map
            ( Data.Bifunctor.second
                (fmap (\res -> Result (Rel (value res) target, entityOf res)))
            )
            res

tryGetComponentsMaybe :: forall c. (Component c) => World -> [ArchetypeId] -> IO [(Entity, Maybe (Result c))]
tryGetComponentsMaybe world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return []
      Just componentId ->
        tryGetComponentsFromTablesMaybe world.tables archetypes componentId

tryGetTicks :: TypeRep -> World -> [ArchetypeId] -> IO (Maybe [Maybe ComponentTicks])
tryGetTicks rep world archetypes =
  do
    componentId <- getComponentId rep world.components
    case componentId of
      Nothing -> return Nothing
      Just componentId ->
        Just <$> tryGetTicksFromTables world.tables archetypes componentId

isAlive :: forall m w. (MonadSystem w m) => Entity -> m Bool
isAlive entity = do
  world <- unsafeGetWorld
  liftIO $ isAliveIO entity world.entities

expect :: (HasCallStack) => forall m w a. (MonadSystem w m) => Text -> Maybe a -> m a
expect t a = withFrozenCallStack $ do
  case a of
    Nothing -> panic t >>= const undefined
    Just x -> return x