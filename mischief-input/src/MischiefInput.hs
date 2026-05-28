module MischiefInput where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.Map (Map)
import GHC.Generics (Generic)
import MischiefECS
import SDL3

class InputType a where
  type PhysicalKeys a
  type VirtualKeys a

newtype Input a = Input {inputs :: Map (VirtualKeys a) InputState}

data InputState = InputState {pressed :: Bool, justPressed :: Bool, justReleased :: Bool}

-- data Key = Enter | Space | Unknown

-- mapSDLKey :: Key -> Maybe SDLKeycode
-- mapSDLKey Enter = Enter

pattern Enter = SDLKReturn

pattern Space = SDLKSpace

newtype KeyManager = KeyManager
  { events :: [SDLKeyboardEvent]
  }
  deriving (Generic, Default, Show, Eq, Queryable)

instance Component KeyManager where
  type Storage KeyManager = ResourceStorage

inputPlugin :: Plugin ()
inputPlugin = do
  addSystem Startup $
    insertResource (def @KeyManager)

  addSystem Update handleInput

handleInput :: System ()
handleInput = do
  -- reset event list in resource
  insertResource (def @KeyManager)
  handleInputEvents

handleInputEvents :: System ()
handleInputEvents = do
  maybeEvent <- liftIO sdlPollEvent
  case maybeEvent of
    Nothing -> do
      return ()
    Just event -> do
      handleInputEvent event
      handleInputEvents

handleInputEvent :: SDLEvent -> System ()
handleInputEvent event = case event of
  SDLEventKeyboard event -> do
    modifyResource (\KeyManager {events} -> KeyManager {events = event : events})
  _ -> return ()

-- liftIO $ print event