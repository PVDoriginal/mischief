module MischiefECS.App.Systems where

import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.StableName (StableName, eqStableName, hashStableName, makeStableName)
import MischiefECS.App.Schedules
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query
import MischiefECS.World.Spawn

data Systems = Systems
  { systemMap :: IORef (Map (ScheduleLabel, Int) (IORef [(StableName (System ()), SystemId)])),
    systemData :: IORef (Map SystemId (System (), IORef Tick)),
    counter :: IORef Int
  }

newtype SystemFunction = SystemFunction {inner :: System ()} deriving anyclass (Component, Queryable)

newtype SystemTick = SystemTick {inner :: IORef Tick} deriving anyclass (Component, Queryable)

newSystems :: IO Systems
newSystems = do
  systemMap <- newIORef Map.empty
  systemData <- newIORef Map.empty
  counter <- newIORef 0
  return Systems {systemMap, systemData, counter}

getSystemId' :: System () -> StableName (System ()) -> IORef [(StableName (System ()), SystemId)] -> IORef Int -> IORef (Map SystemId (System (), IORef Tick)) -> System SystemId
getSystemId' system stableName list counter systemData = do
  list' <- liftIO $ readIORef list
  case find (\x -> fst x `eqStableName` stableName) list' of
    Just (_, x) -> return x
    Nothing -> do
      tick <- liftIO $ newIORef $ Tick 0
      index <- spawn (SystemFunction system, SystemTick tick)
      liftIO $ modifyIORef' list (++ [(stableName, SystemId index)])
      -- tick <- newIORef $ Tick 0
      -- modifyIORef' systemData $ Map.insert (SystemId index) (system, tick)

      return $ SystemId index

getSystemId :: ScheduleLabel -> System () -> Systems -> System SystemId
getSystemId label system Systems {systemMap, counter, systemData} = do
  stableName <- liftIO $ makeStableName system
  systemMap' <- liftIO $ readIORef systemMap

  case Map.lookup (label, hashStableName stableName) systemMap' of
    Just x -> getSystemId' system stableName x counter systemData
    Nothing -> do
      l <- liftIO $ newIORef []
      liftIO $ modifyIORef' systemMap (Map.insert (label, hashStableName stableName) l)
      getSystemId' system stableName l counter systemData

getSystemData :: SystemId -> Systems -> System (System (), IORef Tick)
getSystemData systemId Systems {systemData} = do
  -- systemData <- liftIO $ readIORef systemData
  -- maybe undefined return (Map.lookup systemId systemData)
  Just f <- get @SystemFunction systemId.entity
  Just t <- get @SystemTick systemId.entity
  return (f.inner, t.inner)