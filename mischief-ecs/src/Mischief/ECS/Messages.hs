{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Messages
  ( Message,
    add,
    write,
    read,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.Data (Typeable)
import Data.IORef
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Mischief.ECS.App
import Mischief.ECS.App.SystemDef
import Mischief.ECS.App.Systems
import Mischief.ECS.Components
import Mischief.ECS.Log
import Mischief.ECS.Resources
import Mischief.ECS.Tables
import Mischief.ECS.Utils
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Modify
import Mischief.ECS.World.Query (get)
import Mischief.ECS.World.Query.Queryable
import Prelude hiding (read)

-- | Message typeclass.
class (Typeable m) => Message m

-- | Resource for writing and reading messages.
--
-- Internally, this keeps track of which messages each system has already read.
data Messages m = Messages {messages :: [(Frame, Tick, m)], readers :: Map SystemId Reader}

newMessages :: forall m. Messages m
newMessages = Messages {messages = [], readers = Map.empty}

newtype Reader = Reader (IORef Tick)

getReader :: (Message m) => Messages m -> System Reader
getReader !m = do
  world <- unsafeGetWorld
  case Map.lookup world.systemId m.readers of
    Just r -> return r
    Nothing -> do
      tick <- liftIO $ newIORef $ Tick (0, 0)
      insertRes $ (\Messages {messages, readers} -> Messages {messages, readers = Map.insert world.systemId (Reader tick) readers}) m
      return $ Reader tick

instance (Message m) => Component (Messages m)

-- | Write a message.
write :: forall m. (Message m) => m -> System ()
write !message = do
  messages <- resOrInsert $ newMessages @m
  world <- unsafeGetWorld
  frame <- liftIO $ readIORef world.frame

  loc <- self
  Just currentSystemTick <- get (C @SystemTick) loc

  let message' = (frame, currentSystemTick.inner, message)
  modify messages (\Messages {messages, readers} -> Messages {messages = message' : messages, readers})
  clearOldMessages messages

-- | Read all the messages that haven't been read by the current system.
read :: forall m. (Message m) => System [m]
read = do
  m <- res @(Messages m)
  case m of
    Nothing -> pure []
    Just m -> do
      Reader tick <- getReader m
      readerTick <- liftIO $ readIORef tick

      loc <- self
      Just currentSystemTick <- get (C @SystemTick) loc

      let newMessages = map (\(_, _, x) -> x) $ filter (\(_, tick, _) -> tick < currentSystemTick.inner && tick > readerTick) m.messages
      liftIO $ writeIORef tick currentSystemTick.inner

      return newMessages

-- | Register a new message type with the App. This will automatically create a corresponding resource.
add :: forall (m :: Type). (Message m) => System ()
add = insertRes $ newMessages @m

clearOldMessages :: (Message m) => Result (Messages m) -> System ()
clearOldMessages !m = do
  world <- unsafeGetWorld
  frame <- liftIO $ readIORef world.frame
  modify m (\Messages {messages, readers} -> Messages {messages = filter (\(Frame x, _, _) -> Frame (x + 2) >= frame) messages, readers})