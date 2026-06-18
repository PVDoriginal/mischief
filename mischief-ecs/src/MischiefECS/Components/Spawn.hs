module MischiefECS.Components.Spawn where

import Control.Monad.IO.Class
import Data.Data
import Data.IORef
import Data.Map qualified as Map
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities.Internal
import MischiefECS.World
import MischiefECS.World.Spawn

-- | Get the id of a component - entity pair. In case the component isn't registered, it will give it a new id.
getOrAddPairId :: Pair -> Components -> System ComponentId
getOrAddPairId (Pair (t, entity)) components = do
  component <- getOrAddComponentId t components
  return $ ComponentId {id = component.id, entity = Just entity}

-- | Get the id of a component. In case the component isn't registered, it will give it a new id.
getOrAddComponentId :: ComponentType -> Components -> System ComponentId
getOrAddComponentId (ComponentType (_ :: Proxy c)) Components {components, archetypes} = do
  innerMap <- liftIO $ readIORef components

  case Map.lookup (typeRep $ Proxy @c) innerMap of
    Just t -> return $ ComponentId {id = t, entity = Nothing}
    Nothing -> do
      liftIO $ putStrLn $ "couldn't find " ++ show (typeRep $ Proxy @c)
      liftIO $ modifyIORef' components $ Map.insert (typeRep $ Proxy @(Meta c)) (Entity 0 0)

      result <- spawn (Meta @c, ComponentType $ Proxy @c)

      liftIO $ modifyIORef' components $ Map.insert (typeRep $ Proxy @c) result

      l <- liftIO $ newIORef Set.empty
      liftIO $ modifyIORef' archetypes $ Map.insert result l

      return $ ComponentId {id = result, entity = Nothing}
