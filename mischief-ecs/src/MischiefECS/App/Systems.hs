{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.App.Systems where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Default
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.StableName (StableName, eqStableName, hashStableName, makeStableName)
import MischiefECS.App.Schedules
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Relationships.Order
import MischiefECS.Tables
import MischiefECS.Utils
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Query
import MischiefECS.World.Query.Queryable
import MischiefECS.World.Spawn

newtype Systems = Systems
  { systemMap :: IORef (Map (ScheduleId, Int) (IORef [(StableName (System ()), SystemId)]))
  }
  deriving anyclass (Component)

newtype SystemFunction = SystemFunction {inner :: System ()} deriving anyclass (Component)

newtype SystemTick = SystemTick {inner :: Tick} deriving anyclass (Component)

newtype LastSystemTick = LastSystemTick {inner :: Tick} deriving anyclass (Component)

newSystems :: IO Systems
newSystems = do
  systemMap <- newIORef Map.empty
  return Systems {systemMap}

getSystemId' :: ScheduleId -> System () -> StableName (System ()) -> IORef [(StableName (System ()), SystemId)] -> System SystemId
getSystemId' schedule system stableName list = do
  list' <- liftIO $ readIORef list
  case find (\x -> fst x `eqStableName` stableName) list' of
    Just (_, x) -> return x
    Nothing -> do
      index <- spawn (SystemFunction system, SystemTick (Tick 0), LastSystemTick (Tick 0), Rel (ScheduledIn, schedule.id))
      liftIO $ modifyIORef' list (++ [(stableName, SystemId index)])
      return $ SystemId index

getSystemId :: ScheduleId -> System () -> System SystemId
getSystemId sch system = do
  Systems {systemMap} <- value . unwrap <$> (res @Systems)

  stableName <- liftIO $ makeStableName system
  systemMap' <- liftIO $ readIORef systemMap

  case Map.lookup (sch, hashStableName stableName) systemMap' of
    Just x -> getSystemId' sch system stableName x
    Nothing -> do
      l <- liftIO $ newIORef []
      liftIO $ modifyIORef' systemMap (Map.insert (sch, hashStableName stableName) l)
      getSystemId' sch system stableName l

systemEntity :: (Schedule sch) => sch -> System () -> System Entity
systemEntity sch s = do
  schId <- scheduleEntity sch
  x <- getSystemId (ScheduleId schId) s
  return x.id

getSystemTicks :: World -> IO (Tick, Tick)
getSystemTicks world = do
  let (SystemId sys) = world.systemId
  runSystem
    ( do
        Just (a, b) <- get @(LastSystemTick, SystemTick) sys
        return (a.inner, b.inner)
    )
    world

self :: forall m w. (MonadSystem w m) => m Entity
self = do
  world <- unsafeGetWorld
  let (SystemId sys) = world.systemId
  return sys

local :: forall c. (Queryable c (Result c), Bundle c) => c -> System (Result c)
local c = do
  loc <- self
  getOrInsert c loc

data ScheduledIn = ScheduledIn deriving (Component)
