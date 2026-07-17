{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.App.Systems where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Default
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import GHC.StableName (StableName, eqStableName, hashStableName, makeStableName)
import GHC.Stack.Types
import Mischief.ECS.App.Schedules
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Required
import Mischief.ECS.Entities
import Mischief.ECS.Log
import Mischief.ECS.Relationships.Order
import Mischief.ECS.Tables
import Mischief.ECS.Utils
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Spawn

newtype Systems = Systems
  { systemMap :: IORef (Map (ScheduleId, Int) (IORef [(StableName (System ()), SystemId)]))
  }
  deriving anyclass (Component)

newtype SystemFunction = SystemFunction {inner :: System ()}

instance Component SystemFunction where
  required = require @(SystemTick, LastSystemTick)

newtype SystemTick = SystemTick {inner :: Tick}
  deriving stock (Generic)
  deriving anyclass (Component, Default)

newtype LastSystemTick = LastSystemTick {inner :: Tick}
  deriving stock (Generic)
  deriving anyclass (Component, Default)

newSystems :: IO Systems
newSystems = do
  systemMap <- newIORef Map.empty
  return Systems {systemMap}

getSystemId' :: (HasCallStack) => ScheduleId -> System () -> StableName (System ()) -> IORef [(StableName (System ()), SystemId)] -> System SystemId
getSystemId' schedule system stableName list = do
  list' <- liftIO $ readIORef list
  case find (\x -> fst x `eqStableName` stableName) list' of
    Just (_, x) -> return x
    Nothing -> do
      index <- spawn (SystemFunction system, Rel (ScheduledIn, schedule.id))
      liftIO $ modifyIORef' list (++ [(stableName, SystemId index)])
      return $ SystemId index

getSystemId :: (HasCallStack) => ScheduleId -> System () -> System SystemId
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

removeSystemFromMap' :: StableName (System ()) -> IORef [(StableName (System ()), SystemId)] -> System ()
removeSystemFromMap' stableName list = do
  liftIO $ modifyIORef' list $ filter (\x -> not $ fst x `eqStableName` stableName)

removeSystemFromMap :: ScheduleId -> System () -> System ()
removeSystemFromMap sch system = do
  Systems {systemMap} <- value . unwrap <$> (res @Systems)

  stableName <- liftIO $ makeStableName system
  systemMap' <- liftIO $ readIORef systemMap

  forM_
    (Map.lookup (sch, hashStableName stableName) systemMap')
    (removeSystemFromMap' stableName)

systemEntity :: (HasCallStack, Schedule sch) => sch -> System () -> System Entity
systemEntity sch s = do
  schId <- scheduleEntity sch
  x <- getSystemId (ScheduleId schId) s
  return x.id

getSystemTicks :: World -> IO (Tick, Tick)
getSystemTicks world = do
  let (SystemId sys) = world.systemId
  runSystem
    ( do
        Just (a, b) <- get (C @LastSystemTick, C @SystemTick) sys
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
