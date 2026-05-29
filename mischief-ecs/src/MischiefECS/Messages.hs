{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Messages
  ( Message,
    Messages,
    writeMessage,
    readMessages,
    registerMessage,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (Reader)
import Data.Data (Typeable)
import Data.IORef
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import MischiefECS.App
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query (Modify (modify), Queryable)

class (Typeable m) => Message m

data Messages m = Messages {messages :: [(Tick, m)], readers :: Map SystemId Reader}

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

instance (Message m) => Component (Messages m) where
  type Storage (Messages m) = ResourceStorage

instance (Message m) => Queryable (Messages m)

writeMessage :: (Message m) => m -> ComponentResult (Messages m) -> System ()
writeMessage !message !messages = do
  world <- ask
  let message' = (world.currentSystemTick, message)
  modify messages (\Messages {messages, readers} -> Messages {messages = message' : messages, readers})
  clearOldMessages messages

readMessages :: (Message m) => ComponentResult (Messages m) -> System [m]
readMessages !m = do
  world <- ask
  Reader tick <- getReader m
  readerTick <- liftIO $ readIORef tick

  let newMessages = map snd $ filter (\(tick, _) -> tick < world.currentSystemTick && tick > readerTick) m.value.messages
  liftIO $ writeIORef tick world.currentSystemTick

  return newMessages

registerMessage :: forall (m :: Type). (Message m) => Plugin ()
registerMessage = do
  addSystem Startup $ do
    insertResource $ newMessages @m

clearOldMessages :: (Message m) => ComponentResult (Messages m) -> System ()
clearOldMessages !m = do
  world <- ask
  return ()

-- TODO: clear old messages!!
-- modify m (\Messages {messages, readers} -> Messages {messages = filter (\(Tick x, _) -> Tick (x + 2) >= world.currentSystemTick) messages, readers})