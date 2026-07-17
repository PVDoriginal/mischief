{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Components.Spawn where

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
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.Hooks
import Mischief.ECS.Components.Required (requireAll)
import Mischief.ECS.Entities
import Mischief.ECS.Relationships
import Mischief.ECS.World
import Mischief.ECS.World.Defer
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Prefs
import Mischief.ECS.World.Spawn

-- | Get the id of a component - entity pair. In case the component isn't registered, it will give it a new id.
getOrAddPairId :: Pair -> System ComponentId
getOrAddPairId (Pair (t, entity)) = do
  component <- getOrAddComponentId t
  return $ ComponentId {id = component.id, entity = Just entity}

-- | Get the id of a component. In case the component isn't registered, it will give it a new id.
getOrAddComponentId :: ComponentType -> System ComponentId
getOrAddComponentId (ComponentType (_ :: Proxy c)) = do
  world <- unsafeGetWorld
  let comp = world.components
  innerMap <- liftIO $ readIORef comp.components

  case Map.lookup (typeRep $ Proxy @c) innerMap of
    Just t -> return $ ComponentId {id = t, entity = Nothing}
    Nothing -> do
      result <- liftIO $ getNewEntity world.entities

      liftIO $ modifyIORef' comp.components $ Map.insert (typeRep $ Proxy @c) result

      -- l <- liftIO $ newIORef Set.empty
      -- liftIO $ modifyIORef' comp.archetypes $ Map.insert result l

      forkPrefs (supressEvents True) $
        spawnEntityByInsert
          result
          ( ( ComponentType $ Proxy @c,
              ( def @ComponentArchetypes,
                def @ComponentPairs
              )
            ),
            Name $
              "Meta entity for " ++ show (typeRep $ Proxy @c)
          )

      when (isExclusiveRel @c) $ do
        insert IsExclusiveRelationship result

      for_ (requireAll @c) $ \(DefaultComponentType (_ :: (Proxy other))) -> do
        other <- getOrAddComponentId (ComponentType $ Proxy @other)
        insert (Rel RequiredBy result) other.id
        insert (Rel Requires other.id) result
        insert (DefaultValue $ ErasedComponent $ def @other) other.id

      registerHooks $ hooks @c

      return $ ComponentId {id = result, entity = Nothing}

-- l <- liftIO $ newIORef Set.empty
-- liftIO $ modifyIORef' archetypes $ Map.insert id l

meta :: forall c. (Component c) => System Entity
meta = do
  component <- getOrAddComponentId (ComponentType $ Proxy @c)
  return component.id