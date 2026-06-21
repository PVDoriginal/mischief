{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components.Spawn where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Data
import Data.Default
import Data.Foldable
import Data.IORef
import Data.Kind
import Data.Map qualified as Map
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Required (requireAll)
import MischiefECS.Entities
import MischiefECS.Entities.Internal
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Spawn

-- | Get the id of a component - entity pair. In case the component isn't registered, it will give it a new id.
getOrAddPairId :: Pair -> Components -> System ComponentId
getOrAddPairId (Pair (t, entity)) components = do
  component <- getOrAddComponentId t components
  return $ ComponentId {id = component.id, entity = Just entity}

-- | Get the id of a component. In case the component isn't registered, it will give it a new id.
getOrAddComponentId :: ComponentType -> Components -> System ComponentId
getOrAddComponentId (ComponentType (_ :: Proxy c)) comp = do
  innerMap <- liftIO $ readIORef comp.components

  case Map.lookup (typeRep $ Proxy @c) innerMap of
    Just t -> return $ ComponentId {id = t, entity = Nothing}
    Nothing -> do
      liftIO $ modifyIORef' comp.components $ Map.insert (typeRep $ Proxy @(Meta c)) (Entity 0 0)

      world <- ask
      result <- liftIO $ getNewEntity world.entities

      liftIO $ modifyIORef' comp.components $ Map.insert (typeRep $ Proxy @c) result

      l <- liftIO $ newIORef Set.empty
      liftIO $ modifyIORef' comp.archetypes $ Map.insert result l

      addMetaComponent (Proxy @c) comp
      spawnEntity result ((ComponentType $ Proxy @c, Meta @c), Name $ "Meta entity for " ++ show (typeRep $ Proxy @c))

      for_ (requireAll @c) $ \(DefaultComponentType (_ :: (Proxy other))) -> do
        other <- getOrAddComponentId (ComponentType $ Proxy @other) comp
        insert (R (RequiredBy, result)) other.id
        insert (R (Requires, other.id)) result
        insert (DefaultValue $ ErasedComponent $ def @other) other.id

      return $ ComponentId {id = result, entity = Nothing}

addMetaComponent :: forall (c :: Type). (Typeable c) => Proxy c -> Components -> System ()
addMetaComponent _ Components {components, archetypes} = do
  world <- ask
  id <- liftIO $ getNewEntity world.entities

  liftIO $ modifyIORef' components $ Map.insert (typeRep $ Proxy @(Meta c)) id

  l <- liftIO $ newIORef Set.empty
  liftIO $ modifyIORef' archetypes $ Map.insert id l

entityOf :: forall c. (Component c) => System Entity
entityOf = do
  world <- ask
  component <- getOrAddComponentId (ComponentType $ Proxy @c) world.components
  return component.id