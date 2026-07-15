module Mischief.ECS.Log where

import Colog qualified
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack
import Mischief.ECS.World

debug :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
debug msg = withFrozenCallStack $ do
  world <- unsafeGetWorld
  liftIO $ Colog.usingLoggerT world.logger $ Colog.logDebug msg

info :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
info msg = withFrozenCallStack $ do
  world <- unsafeGetWorld
  liftIO $ Colog.usingLoggerT world.logger $ Colog.logInfo msg

warn :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
warn msg = withFrozenCallStack $ do
  world <- unsafeGetWorld
  liftIO $ Colog.usingLoggerT world.logger $ Colog.logWarning msg

err :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
err msg = withFrozenCallStack $ do
  world <- unsafeGetWorld
  liftIO $ Colog.usingLoggerT world.logger $ Colog.logError msg

panic :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
panic msg = withFrozenCallStack $ do
  err msg
  undefined

text :: (Show a) => a -> Text
text = Text.pack . show
