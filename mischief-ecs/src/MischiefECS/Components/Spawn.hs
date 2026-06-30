{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components.Spawn where

import Control.Monad
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
import MischiefECS.Relationships
import MischiefECS.World
import MischiefECS.World.Defer
import MischiefECS.World.Insert
import MischiefECS.World.Prefs
import MischiefECS.World.Spawn

-- | Get the id of a component - entity pair. In case the component isn't registered, it will give it a new id.
getOrAddPairId :: Pair -> System ComponentId
getOrAddPairId (Pair (t, entity)) = do
  component <- getOrAddComponentId t
  return $ ComponentId {id = component.id, entity = Just entity}

-- | Get the id of a component. In case the component isn't registered, it will give it a new id.
getOrAddComponentId :: ComponentType -> System ComponentId
getOrAddComponentId (ComponentType (_ :: Proxy c)) = do
  world <- ask
  let comp = world.components
  innerMap <- liftIO $ readIORef comp.components

  case Map.lookup (typeRep $ Proxy @c) innerMap of
    Just t -> return $ ComponentId {id = t, entity = Nothing}
    Nothing -> do
      liftIO $ modifyIORef' comp.components $ Map.insert (typeRep $ Proxy @(Meta c)) (Entity 0 0)

      world <- ask
      result <- liftIO $ getNewEntity world.entities

      liftIO $ modifyIORef' comp.components $ Map.insert (typeRep $ Proxy @c) result

      -- l <- liftIO $ newIORef Set.empty
      -- liftIO $ modifyIORef' comp.archetypes $ Map.insert result l

      addMetaComponent (Proxy @c) comp

      forkPrefs (supressEvents True) $
        spawnEntity
          result
          ( ( ComponentType $ Proxy @c,
              Meta @c
            ),
            Name $ "Meta entity for " ++ show (typeRep $ Proxy @c)
          )

      when (isExclusiveRel @c) $ do
        insert IsExclusiveRelationship result

      for_ (requireAll @c) $ \(DefaultComponentType (_ :: (Proxy other))) -> do
        other <- getOrAddComponentId (ComponentType $ Proxy @other)
        insert (Rel (RequiredBy, result)) other.id
        insert (Rel (Requires, other.id)) result
        insert (DefaultValue $ ErasedComponent $ def @other) other.id

      return $ ComponentId {id = result, entity = Nothing}

addMetaComponent :: forall (c :: Type). (Component c) => Proxy c -> Components -> System ()
addMetaComponent _ Components {components} = do
  world <- ask
  id <- liftIO $ getNewEntity world.entities

  liftIO $ modifyIORef' components $ Map.insert (typeRep $ Proxy @(Meta c)) id

  forkPrefs (supressEvents True) $
    spawnEntity
      id
      ( ComponentType $ Proxy @(Meta c),
        Name $ "Meta entity for " ++ show (typeRep $ Proxy @(Meta c))
      )

-- l <- liftIO $ newIORef Set.empty
-- liftIO $ modifyIORef' archetypes $ Map.insert id l

entityOf :: forall c. (Component c) => System Entity
entityOf = do
  component <- getOrAddComponentId (ComponentType $ Proxy @c)
  return component.id