module MischiefECS.Log where

import Colog qualified
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exception (prettyCallStackLines)
import GHC.Stack
import MischiefECS.World
import System.Console.ANSI

info :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
info msg = withFrozenCallStack $ do
  world <- unsafeGetWorld
  liftIO $ Colog.usingLoggerT world.logger $ Colog.logInfo msg

warn :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
warn msg = withFrozenCallStack $ do
  world <- unsafeGetWorld
  liftIO $ Colog.usingLoggerT world.logger $ Colog.logWarning msg

error :: (HasCallStack) => (MonadSystem w m) => Text -> m ()
error msg = withFrozenCallStack $ do
  world <- unsafeGetWorld
  liftIO $ Colog.usingLoggerT world.logger $ Colog.logError msg

text :: (Show a) => a -> Text
text = Text.pack . show
