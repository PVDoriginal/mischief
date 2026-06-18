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
getOrAddComponentId :: TypeRep -> Components -> System ComponentId
getOrAddComponentId t Components {components, archetypes, counter} = do
  innerMap <- liftIO $ readIORef components

  case Map.lookup t innerMap of
    Just t -> return $ ComponentId {id = t, entity = Nothing}
    Nothing -> do
      result <- liftIO $ readIORef counter
      liftIO $ modifyIORef counter (+ 1)

      liftIO $ modifyIORef components $ Map.insert t result

      l <- liftIO $ newIORef Set.empty
      liftIO $ modifyIORef archetypes $ Map.insert result l

      return $ ComponentId {id = result, entity = Nothing}
