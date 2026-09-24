{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.App where

-- import Data.Map
-- import Data.Map qualified as Map

import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..))
import Data.Data
import Data.Default
import Data.Foldable
import Data.IORef
import Mischief.ECS.App.Plugins
import Mischief.ECS.App.Schedules
import Mischief.ECS.App.SystemDef
import Mischief.ECS.App.Systems (ScheduledIn, SystemFunction, Systems)
import Mischief.ECS.App.Systems qualified as Systems
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Spawn (getOrAddComponentId)
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Hidden
import Mischief.ECS.Log
import Mischief.ECS.Relationships.Order
import Mischief.ECS.Resources
import Mischief.ECS.World
import Mischief.ECS.World.Defer
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Spawn

data App = App
  { world :: World,
    systems :: Systems
  }

newApp :: IO App
newApp = do
  world <- newWorld getTools
  runSystem (spawnEntity (Entity (# 0##, 0## #)) ()) world

  systems <- Systems.newSystems

  let app = App {world, systems}

  runSystem appInit app.world
  -- runSystem (Systems.add Init $ runPluginRec plugin) app.world

  return app

addPlugin :: forall p. (Plugin p) => App -> IO ()
addPlugin app = runSystem (addPluginRec @p) app.world

runApp :: App -> IO ()
runApp app = flip runSystem app.world $ do
  x <- scheduleEntity @Init
  liftIO $ runSchedules [x]

  startups <- orderEntities =<< query (mkQuery' E (With (C @StartupSchedule)))
  updates <- orderEntities =<< query (mkQuery' E (With (C @UpdateSchedule)))

  liftIO $ runSchedules startups
  liftIO $ runSchedulesLoop updates
  where
    runSchedulesLoop schedules = do
      forever $ do
        runSchedules schedules
        modifyIORef' app.world.frame (\(Frame x) -> Frame $ x + 1)

    runSchedules schedules =
      for_ schedules $ \schedule -> do
        runSystem (runSchedule' schedule) app.world

runSchedule :: forall sch. (Schedule sch) => System ()
runSchedule = scheduleEntity @sch >>= runSchedule'

runSchedule' :: Entity -> System ()
runSchedule' schedule = do
  world <- unsafeGetWorld
  systems <- orderEntities =<< query (mkQuery' E (With (R @ScheduledIn schedule)))

  for_ systems $ \systemId -> do
    Just (systemFunction, lastSystemTick) <- single $ mkGet systemId (C @SystemFunction, C @SystemTick)
    currentSystemTick <- liftIO $ readIORef world.tick

    insert (SystemTick currentSystemTick) systemId
    insert (LastSystemTick lastSystemTick.inner) systemId

    Control.Monad.Reader.local (hide . setSystemId (SystemId systemId) . unhide) $ do
      systemFunction.inner
      flush
      flushAsync
      flushEvents
      tick

appInit :: System ()
appInit = do
  insertRes $ def @Schedules

  systems <- liftIO Systems.newSystems
  insertRes systems

  pre <- scheduleEntity @PreStartup
  startup <- scheduleEntity @Startup
  post <- scheduleEntity @PostStartup

  for_ [pre, startup, post] $ insert StartupSchedule

  insert (Rel Before startup) pre
  insert (Rel Before post) startup

  first <- scheduleEntity @First
  pre <- scheduleEntity @PreUpdate
  update <- scheduleEntity @Update
  post <- scheduleEntity @PostUpdate
  last <- scheduleEntity @Last

  for_ [first, pre, update, post, last] $ insert UpdateSchedule

  insert (Rel Before pre) first
  insert (Rel Before update) pre
  insert (Rel Before post) update
  insert (Rel Before last) post

register :: forall c. (Component c) => System ()
register = registerComponent $ Proxy @c

registerComponent :: forall c. (Component c) => Proxy c -> System ()
registerComponent c = do
  _ <- getOrAddComponentId (ComponentType c)
  return ()

getTools :: SystemTools
getTools =
  SystemTools
    { get = toolsGet,
      getRAny = toolsGetRAny,
      set = toolsSet,
      spawnByInsert = toolsSpawnByInsert
    }

toolsGet :: forall c m w. (MonadSystem w m, Component c) => Proxy c -> Entity -> m (Maybe c)
toolsGet _ e = single $ mkGet e (C @c)

toolsSet :: forall c. (Bundle c) => c -> Entity -> System ()
toolsSet = insert

toolsGetRAny :: forall c m w. (Component c, MonadSystem w m, IsExclusiveRel c ~ False) => Proxy c -> Entity -> m (Maybe [Rel c])
toolsGetRAny _ e = single $ mkGet e (R @c Any)

toolsSpawnByInsert :: forall b. (Bundle b) => Entity -> b -> System ()
toolsSpawnByInsert = spawnEntityByInsert

incTick :: Tick -> Maybe Tick
incTick (Tick (a, b)) | a == maxBound && b == maxBound = Nothing
incTick (Tick (a, b)) | b == maxBound = Just $ Tick (a + 1, 0)
incTick (Tick (a, b)) = Just $ Tick (a, b + 1)

-- | Increment the World's Tick.
tick :: System ()
tick = do
  world <- unsafeGetWorld
  tick <- liftIO $ incTick <$> readIORef world.tick
  case tick of
    Nothing -> do
      warn "Reached maximum Tick. Resetting count. Previous changed will not be detected."
      liftIO $ writeIORef world.tick $ Tick (0, 0)
    Just t -> liftIO $ writeIORef world.tick t
