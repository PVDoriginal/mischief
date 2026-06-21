{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Messages
  ( Message,
    Messages,
    writeMessage,
    readMessages,
    addMessage,
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
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Modify
import MischiefECS.World.Query (Queryable)

class (Typeable m) => Message m

data Messages m = Messages {messages :: [(Frame, Tick, m)], readers :: Map SystemId Reader}

newMessages :: forall m. Messages m
newMessages = Messages {messages = [], readers = Map.empty}

newtype Reader = Reader (IORef Tick)

getReader :: (Message m) => ComponentResult (Messages m) -> System Reader
getReader !m = do
  world <- ask
  case Map.lookup world.systemId m.value.readers of
    Just r -> return r
    Nothing -> do
      tick <- liftIO $ newIORef $ Tick 0
      modify m (\Messages {messages, readers} -> Messages {messages, readers = Map.insert world.systemId (Reader tick) readers})
      return $ Reader tick

instance (Message m) => Component (Messages m)

instance (Message m) => Queryable (Messages m)

writeMessage :: (Message m) => m -> ComponentResult (Messages m) -> System ()
writeMessage !message !messages = do
  world <- ask
  frame <- liftIO $ readIORef world.frame
  let message' = (frame, world.currentSystemTick, message)
  modify messages (\Messages {messages, readers} -> Messages {messages = message' : messages, readers})
  clearOldMessages messages

readMessages :: (Message m) => ComponentResult (Messages m) -> System [m]
readMessages !m = do
  world <- ask
  Reader tick <- getReader m
  readerTick <- liftIO $ readIORef tick

  let newMessages = map (\(_, _, x) -> x) $ filter (\(_, tick, _) -> tick < world.currentSystemTick && tick > readerTick) m.value.messages
  liftIO $ writeIORef tick world.currentSystemTick

  return newMessages

addMessage :: forall (m :: Type). (Message m) => Plugin ()
addMessage = do
  addRes $ newMessages @m

clearOldMessages :: (Message m) => ComponentResult (Messages m) -> System ()
clearOldMessages !m = do
  world <- ask
  frame <- liftIO $ readIORef world.frame
  modify m (\Messages {messages, readers} -> Messages {messages = filter (\(Frame x, _, _) -> Frame (x + 2) >= frame) messages, readers})