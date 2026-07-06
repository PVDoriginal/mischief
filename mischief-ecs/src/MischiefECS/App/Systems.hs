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
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Internal
import MischiefECS.World.Query
import MischiefECS.World.Spawn

newtype Systems = Systems
  { systemMap :: IORef (Map (ScheduleLabel, Int) (IORef [(StableName (System ()), SystemId)]))
  }

newtype SystemFunction = SystemFunction {inner :: System ()} deriving anyclass (Component)

newtype SystemTick = SystemTick {inner :: Tick} deriving anyclass (Component)

newtype LastSystemTick = LastSystemTick {inner :: Tick} deriving anyclass (Component)

newSystems :: IO Systems
newSystems = do
  systemMap <- newIORef Map.empty
  return Systems {systemMap}

getSystemId' :: System () -> StableName (System ()) -> IORef [(StableName (System ()), SystemId)] -> System SystemId
getSystemId' system stableName list = do
  list' <- liftIO $ readIORef list
  case find (\x -> fst x `eqStableName` stableName) list' of
    Just (_, x) -> return x
    Nothing -> do
      index <- spawn (SystemFunction system, (SystemTick (Tick 0), LastSystemTick (Tick 0)))
      liftIO $ modifyIORef' list (++ [(stableName, SystemId index)])
      return $ SystemId index

getSystemId :: ScheduleLabel -> System () -> Systems -> System SystemId
getSystemId label system Systems {systemMap} = do
  stableName <- liftIO $ makeStableName system
  systemMap' <- liftIO $ readIORef systemMap

  case Map.lookup (label, hashStableName stableName) systemMap' of
    Just x -> getSystemId' system stableName x
    Nothing -> do
      l <- liftIO $ newIORef []
      liftIO $ modifyIORef' systemMap (Map.insert (label, hashStableName stableName) l)
      getSystemId' system stableName l

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
  world <- asks getWorld
  let (SystemId sys) = world.systemId
  return sys

local :: forall c. (Queryable c, QueryOutput c ~ Result c, Bundle c) => c -> System (Result c)
local c = do
  loc <- self
  getOrInsert c loc