module MischiefInput where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (def))
import GHC.Generics (Generic)
import MischiefECS
import SDL3

-- data Key = Enter | Space | Unknown

-- mapSDLKey :: Key -> Maybe SDLKeycode
-- mapSDLKey Enter = Enter

pattern Enter = SDLKReturn

pattern Space = SDLKSpace

newtype KeyManager = KeyManager
  { events :: [SDLKeyboardEvent]
  }
  deriving (Generic, Default, Show, Eq, Queryable)

isPressed :: SDLKeycode -> KeyManager -> Bool
isPressed key manager =
  any
    ( \event ->
        (event.sdlKeyboardKey == key) && event.sdlKeyboardDown
    )
    manager.events

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