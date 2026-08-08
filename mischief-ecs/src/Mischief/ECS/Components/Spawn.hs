{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Components.Spawn where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Data
import Data.Default
import Data.Foldable
-- import Mischief.ECS.Components.Hooks
-- import Mischief.ECS.World.Defer
-- import Mischief.ECS.World.Insert

-- import Mischief.ECS.Events

import Data.HashTable.IO qualified as H
import Data.IORef
import Data.Kind
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Base (Word (..))
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.HooksDef
import Mischief.ECS.Components.Required (requireAll)
import Mischief.ECS.Entities
import Mischief.ECS.EntityDef
import Mischief.ECS.Observer
import Mischief.ECS.Relationships
import Mischief.ECS.World
import Mischief.ECS.World.Prefs

-- import Mischief.ECS.World.Spawn

-- | Get the id of a component - entity pair. In case the component isn't registered, it will give it a new id.
getOrAddPairId :: Pair -> System ComponentId
getOrAddPairId (Pair (t, entity)) = do
  (ComponentId (# id, _ #)) <- getOrAddComponentId t
  return $ ComponentId (# id, Just entity #)

-- | Get the id of a component. In case the component isn't registered, it will give it a new id.
getOrAddComponentId :: ComponentType -> System ComponentId
getOrAddComponentId (ComponentType (_ :: Proxy c)) = do
  world <- unsafeGetWorld
  let comp = world.components
  w <- liftIO $ H.lookup comp.components (typeRep $ Proxy @c)

  case w of
    Just (W# t) -> return $ ComponentId (# t, Nothing #)
    Nothing -> do
      result <- liftIO $ getNewEntityComp world.entities
      let !(Entity (# id, _ #)) = result

      liftIO $ H.insert comp.components (typeRep $ Proxy @c) (W# id)
      -- liftIO $ modifyIORef' comp.components $ Map.insert (typeRep $ Proxy @c) (W# id)

      -- l <- liftIO $ newIORef Set.empty
      -- liftIO $ modifyIORef' comp.archetypes $ Map.insert result l

      forkPrefs (supressEvents True) $
        worldSpawnByInsert
          result
          ( ( ComponentType $ Proxy @c,
              ( def @ComponentArchetypes,
                def @ComponentPairs
              )
            ),
            Name $
              "Meta entity for " ++ show (typeRep $ Proxy @c)
          )

      when (isExclusive @(RelExclusivity c)) $ do
        worldSet IsExclusiveRelationship result

      for_ (requireAll @c) $ \(DefaultComponentType (_ :: (Proxy other))) -> do
        (ComponentId (# otherId', _ #)) <- getOrAddComponentId (ComponentType $ Proxy @other)
        let otherId = Entity (# otherId', 0## #)
        worldSet (Rel RequiredBy result) otherId
        worldSet (Rel Requires otherId) result
        worldSet (DefaultValue $ ErasedComponent $ def @other) otherId

      registerHooks $ hooks @c

      return $ ComponentId (# id, Nothing #)

-- l <- liftIO $ newIORef Set.empty
-- liftIO $ modifyIORef' archetypes $ Map.insert id l

meta :: forall c. (Component c) => System Entity
meta = do
  (ComponentId (# id, _ #)) <- getOrAddComponentId (ComponentType $ Proxy @c)
  return $ Entity (# id, 0## #)

tryMeta :: forall c m w. (Component c, MonadSystem w m) => m (Maybe Entity)
tryMeta = do
  world <- unsafeGetWorld
  component <- liftIO $ getComponentId (typeRep $ Proxy @c) world.components
  return $ fmap (\(ComponentId (# id, _ #)) -> Entity (# id, 0## #)) component

registerHooks :: Hooks c -> System ()
registerHooks (Hooks h) = for_ h registerHook

registerHook :: ErasedHook c -> System ()
registerHook (ErasedHook (h :: e c -> m ())) = do
  world <- unsafeGetWorld
  e <- liftIO $ getNewEntity world.entities
  case eqT @m @System of
    Just Refl -> void $ worldSpawnByInsert e $ Observer h
    Nothing -> undefined
