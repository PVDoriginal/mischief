module MischiefECS.App.Systems where

import Data.Foldable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.StableName (StableName, eqStableName, hashStableName, makeStableName)
import MischiefECS.App.Schedules
import MischiefECS.Components
import MischiefECS.World

data Systems = Systems
  { systemMap :: IORef (Map (ScheduleLabel, Int) (IORef [(StableName (System ()), SystemId)])),
    systemData :: IORef (Map SystemId (System (), IORef Tick)),
    counter :: IORef Int
  }

newSystems :: IO Systems
newSystems = do
  systemMap <- newIORef Map.empty
  systemData <- newIORef Map.empty
  counter <- newIORef 0
  return Systems {systemMap, systemData, counter}

getSystemId' :: System () -> StableName (System ()) -> IORef [(StableName (System ()), SystemId)] -> IORef Int -> IORef (Map SystemId (System (), IORef Tick)) -> IO SystemId
getSystemId' system stableName list counter systemData = do
  list' <- readIORef list
  case find (\x -> fst x `eqStableName` stableName) list' of
    Just (_, x) -> return x
    Nothing -> do
      index <- readIORef counter
      modifyIORef' counter (+ 1)
      modifyIORef' list (++ [(stableName, SystemId index)])
      tick <- newIORef $ Tick 0
      modifyIORef' systemData $ Map.insert (SystemId index) (system, tick)
      return $ SystemId index

getSystemId :: ScheduleLabel -> System () -> Systems -> IO SystemId
getSystemId label system Systems {systemMap, counter, systemData} = do
  stableName <- makeStableName system
  systemMap' <- readIORef systemMap

  case Map.lookup (label, hashStableName stableName) systemMap' of
    Just x -> getSystemId' system stableName x counter systemData
    Nothing -> do
      l <- newIORef []
      modifyIORef' systemMap (Map.insert (label, hashStableName stableName) l)
      getSystemId' system stableName l counter systemData

getSystemData :: SystemId -> Systems -> IO (System (), IORef Tick)
getSystemData systemId Systems {systemData} = do
  systemData <- readIORef systemData
  maybe undefined return (Map.lookup systemId systemData)