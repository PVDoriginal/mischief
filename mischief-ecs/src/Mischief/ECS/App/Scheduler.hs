module Mischief.ECS.App.Scheduler where

import Data.Functor
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Mischief.ECS.App.Schedules
import Mischief.ECS.Components
import Mischief.ECS.Graph (Graph)
import Mischief.ECS.Graph qualified as Graph
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable

-- data Scheduler = Scheduler
--   { startup :: Graph ScheduleLabel,
--     update :: Graph ScheduleLabel,
--     systems :: IORef (Map ScheduleLabel (Graph SystemId))
--   }
--   deriving (Component)

-- -- data ScheduleType = StartupSchedule | UpdateSchedule

-- data StartupSchedule = StartupSchedule deriving (Component)

-- data UpdateSchedule = UpdateSchedule deriving (Component)

-- data ScheduleId =

-- newScheduler :: IO Scheduler
-- newScheduler = do
--   startup <- Graph.newGraph
--   update <- Graph.newGraph
--   systems <- newIORef Map.empty
--   return Scheduler {startup, update, systems}

-- addSchedule :: ScheduleLabel -> ScheduleType -> Scheduler -> IO ()
-- addSchedule label StartupSchedule Scheduler {startup} = Graph.addNode label startup $> ()
-- addSchedule label UpdateSchedule Scheduler {update} = Graph.addNode label update $> ()

-- addScheduleEdge :: (ScheduleLabel, ScheduleLabel) -> ScheduleType -> Scheduler -> IO ()
-- addScheduleEdge edge StartupSchedule Scheduler {startup} = Graph.addEdge edge startup
-- addScheduleEdge edge UpdateSchedule Scheduler {update} = Graph.addEdge edge update

-- getSchedules :: ScheduleType -> Scheduler -> IO [ScheduleLabel]
-- getSchedules StartupSchedule Scheduler {startup} = do
--   nodes <- Graph.getNodes startup
--   return $ concat nodes
-- getSchedules UpdateSchedule Scheduler {update} = do
--   nodes <- Graph.getNodes update
--   return $ concat nodes

-- getScheduleGraph :: ScheduleLabel -> Scheduler -> IO (Graph SystemId)
-- getScheduleGraph label Scheduler {systems} = do
--   systems' <- readIORef systems
--   case Map.lookup label systems' of
--     Just x -> return x
--     Nothing -> do
--       graph <- Graph.newGraph
--       modifyIORef' systems $ Map.insert label graph
--       return graph

-- addSystem :: ScheduleLabel -> SystemId -> Scheduler -> IO ()
-- addSystem label systemId scheduler = do
--   graph <- getScheduleGraph label scheduler
--   Graph.addNode systemId graph $> ()

-- addSystemEdge :: ScheduleLabel -> (SystemId, SystemId) -> Scheduler -> IO ()
-- addSystemEdge label edge scheduler = do
--   graph <- getScheduleGraph label scheduler
--   Graph.addEdge edge graph

-- getScheduleSystems :: ScheduleLabel -> Scheduler -> IO [[SystemId]]
-- getScheduleSystems label scheduler = do
--   graph <- getScheduleGraph label scheduler
--   Graph.getNodes graph