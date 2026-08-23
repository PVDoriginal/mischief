module Mischief.ECS.Events where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.Data (Typeable)
import Data.Default (Default)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.List
import GHC.Generics (Generic)
import GHC.Records
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Required (require)
import Mischief.ECS.Entities
import Mischief.ECS.EventDef
import Mischief.ECS.Observer
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.Queryable

trigger :: (Event e) => e -> System ()
trigger event = do
  world <- unsafeGetWorld
  liftIO $ modifyIORef' world.events (++ [eraseEvent event])

flushEvents :: System ()
flushEvents = do
  world <- unsafeGetWorld
  events <- liftIO $ readIORef world.events
  for_ events runEvent
  liftIO $ writeIORef world.events []

runEvent :: ErasedEvent -> System ()
runEvent (ErasedEvent (event :: e)) = do
  observers' <- query (C @(Observer e), C @(EventProxy e), C @ObserverOrder)
  let observers = sortBy (\(_, _, a) (_, _, b) -> compare a b) observers'
  for_ observers $ \(observer, _, _) -> do
    let Observer f = value observer
    f event

newtype OnInsert c = OnSet {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

data OnInsertRel c = OnSetRel {entity :: Entity, target :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

newtype OnAdd c = OnAdd {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

data OnAddRel c = OnAddRel {entity :: Entity, target :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

newtype OnRemove c = OnRemove {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

data OnRemoveRel c = OnRemoveRel {entity :: Entity, target :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)
