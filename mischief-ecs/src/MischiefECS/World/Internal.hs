module MischiefECS.World.Internal where

import Control.Monad.Reader
import Data.IORef (IORef)
import MischiefECS.World

data ParWorld = ParWorld {world :: World, deferred :: IORef [System ()]}

newtype ParSystem a = ParSystem (ReaderT ParWorld IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ParWorld, MonadFail)

class GetWorld w where
  getWorld :: w -> World

instance GetWorld World where
  getWorld = id

instance GetWorld ParWorld where
  getWorld w = w.world

class (GetWorld w, MonadReader w a, Applicative a, MonadFail a, Functor a, Monad a, MonadIO a) => MonadSystem w a

instance MonadSystem World System

instance MonadSystem ParWorld ParSystem
