{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Messages
  ( Message,
    Messages,
    addMessage,
    writeMessage,
    readMessages,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.Data (Typeable)
import Data.IORef
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import MischiefECS.App
import MischiefECS.App.Systems
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.Utils
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Modify
import MischiefECS.World.Query (get)
import MischiefECS.World.Query.Queryable

-- | Message typeclass.
class (Typeable m) => Message m

-- | Resource for writing and reading messages.
--
-- Internally, this keeps track of which messages each system has already read.
data Messages m = Messages {messages :: [(Frame, Tick, m)], readers :: Map SystemId Reader}

newMessages :: forall m. Messages m
newMessages = Messages {messages = [], readers = Map.empty}

newtype Reader = Reader (IORef Tick)

getReader :: (Message m) => Result (Messages m) -> System Reader
getReader !m = do
  world <- unsafeGetWorld
  case Map.lookup world.systemId m.readers of
    Just r -> return r
    Nothing -> do
      tick <- liftIO $ newIORef $ Tick 0
      modify m (\Messages {messages, readers} -> Messages {messages, readers = Map.insert world.systemId (Reader tick) readers})
      return $ Reader tick

instance (Message m) => Component (Messages m)

instance (Message m) => Queryable (Messages m)

-- | Write a message into the resource.
writeMessage :: (Message m) => m -> Result (Messages m) -> System ()
writeMessage !message !messages = do
  world <- unsafeGetWorld
  frame <- liftIO $ readIORef world.frame

  loc <- self
  Just currentSystemTick <- get @SystemTick loc

  let message' = (frame, currentSystemTick.inner, message)
  modify messages (\Messages {messages, readers} -> Messages {messages = message' : messages, readers})
  clearOldMessages messages

-- | Read all the messages from this resource that haven't been read by the current system.
readMessages :: (Message m) => Result (Messages m) -> System [m]
readMessages !m = do
  Reader tick <- getReader m
  readerTick <- liftIO $ readIORef tick

  loc <- self
  Just currentSystemTick <- get @SystemTick loc

  let newMessages = map (\(_, _, x) -> x) $ filter (\(_, tick, _) -> tick < currentSystemTick.inner && tick > readerTick) m.messages
  liftIO $ writeIORef tick currentSystemTick.inner

  return newMessages

-- | Register a new message type with the App. This will automatically create a corresponding resource.
addMessage :: forall (m :: Type). (Message m) => System ()
addMessage = insertRes $ newMessages @m

clearOldMessages :: (Message m) => Result (Messages m) -> System ()
clearOldMessages !m = do
  world <- unsafeGetWorld
  frame <- liftIO $ readIORef world.frame
  modify m (\Messages {messages, readers} -> Messages {messages = filter (\(Frame x, _, _) -> Frame (x + 2) >= frame) messages, readers})