{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Messages where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (Reader)
import Data.Data (Proxy (Proxy), TypeRep, Typeable)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import MischiefECS.App
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.World

class (Typeable m) => Message m

data Messages m = Messages {messages :: [(Tick, m)], readers :: Map SystemId Reader, _proxy :: Proxy m}

newMessages :: forall m. (Message m) => Proxy m -> Messages m
newMessages proxy = Messages {messages = [], readers = Map.empty, _proxy = proxy}

newtype Reader = Reader (IORef Tick)

getReader :: (Message m) => ComponentResult (Messages m) -> System Reader
getReader m = do
  world <- ask
  case Map.lookup world.systemId m.value.readers of
    Just r -> return r
    Nothing -> do
      tick <- liftIO $ newIORef $ Tick 0
      modify m (\Messages {messages, readers} -> Messages {messages, readers = Map.insert world.systemId (Reader tick) readers})
      return $ Reader tick

instance (Message m) => Component (Messages m) where
  type Storage (Messages m) = ResourceStorage

writeMessage :: (Message m) => m -> ComponentResult (Messages m) -> System ()
writeMessage message messages = do
  world <- ask
  let message' = (world.currentSystemTick, message)
  modify messages (\Messages {messages, readers} -> Messages {messages = message' : messages, readers})

readMessages :: (Message m) => ComponentResult (Messages m) -> System [m]
readMessages m = do
  world <- ask
  Reader tick <- getReader m
  readerTick <- liftIO $ readIORef tick

  let newMessages = map snd $ filter (\(tick, _) -> tick < world.currentSystemTick && tick > readerTick) m.value.messages
  liftIO $ writeIORef tick world.currentSystemTick

  return newMessages

-- registerMessage :: forall m. (Message m) => Plugin ()
-- registerMessage m = do
--   addSystem Startup $ do
--     insertResource $ newMessages @m

aa :: forall m. (Message m) => System ()
aa = do
  -- let x = newMessages (Proxy @m)
  -- undefined

  -- insertResource x

  return ()