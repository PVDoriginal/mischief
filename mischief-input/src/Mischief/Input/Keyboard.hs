module Mischief.Input.Keyboard where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import Mischief.ECS
import Mischief.ECS.Messages qualified as Messages
import Mischief.ECS.Systems qualified as Systems
import Mischief.SDL (SDLMessage (..), SDLPlugin (..))
import SDL3.Sys qualified as SDL3

data Keys = Keys
  { physical :: Map SDL3.SDL_Scancode KeyState,
    virtual :: Map SDL3.SDL_Keycode KeyState
  }
  deriving (Generic, Default, Component)

newtype HotKeys = HotKeys [((SDL3.SDL_Scancode, SDL3.SDL_Keycode), KeyState)] deriving (Generic, Default, Component)

pressed :: SDL3.SDL_Scancode -> Keys -> Bool
pressed scancode keys = case Map.lookup scancode keys.physical of
  Just Pressed -> True
  Just JustPressed -> True
  _ -> False

justPressed :: SDL3.SDL_Scancode -> Keys -> Bool
justPressed scancode keys = case Map.lookup scancode keys.physical of
  Just JustPressed -> True
  _ -> False

justReleased :: SDL3.SDL_Scancode -> Result Keys -> Bool
justReleased scancode keys = case Map.lookup scancode keys.physical of
  Just JustReleased -> True
  _ -> False

getState :: SDL3.SDL_Scancode -> IORef Keys -> IO KeyState
getState scancode keys' = do
  keys <- readIORef keys'
  case Map.lookup scancode keys.physical of
    Just x -> return x
    Nothing -> do
      modifyIORef' keys' (\Keys {physical, virtual} -> Keys {physical = Map.insert scancode Released physical, virtual})
      return Released

setState :: KeyState -> (SDL3.SDL_Scancode, SDL3.SDL_Keycode) -> IORef Keys -> IO ()
setState state (scancode, keycode) keys =
  modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical = Map.insert scancode state physical, virtual = Map.insert keycode state virtual})

setStatePhysical :: KeyState -> SDL3.SDL_Scancode -> IORef Keys -> IO ()
setStatePhysical state scancode keys =
  modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical = Map.insert scancode state physical, virtual})

setStateVirtual :: KeyState -> SDL3.SDL_Keycode -> IORef Keys -> IO ()
setStateVirtual state keycode keys =
  modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical, virtual = Map.insert keycode state virtual})

data KeyState = Pressed | JustPressed | Released | JustReleased
  deriving (Show, Eq)

data KeyboardPlugin = KeyboardPlugin deriving (Eq)

instance Plugin KeyboardPlugin where
  init _ = do
    insertRes (def @Keys)
    insertRes (def @HotKeys)
    Systems.add Update readEvents

  plugins _ = plug SDLPlugin

readEvents :: System ()
readEvents = do
  messages <- Messages.read @(SDLMessage SDL3.SDL_KeyboardEvent)

  Just keys <- res @Keys

  keys <- liftIO $ newIORef keys
  hotKeys <- liftIO $ newIORef []
  processedKeys <- liftIO $ newIORef []

  for_ messages $ \(SDLMessage {eventType, event}) -> do
    state <- liftIO $ getState event.scancode keys
    let state' = updateState (Just eventType) state
    liftIO $ setState state' (event.scancode, event.key) keys

    when (state' == JustReleased || state' == JustPressed) $
      liftIO $
        modifyIORef' hotKeys (++ [((event.scancode, event.key), state')])

    liftIO $ modifyIORef' processedKeys (++ [(event.scancode, event.key)])

  Just (HotKeys oldHotKeys) <- res @HotKeys
  processedKeys <- liftIO $ readIORef processedKeys

  for_ oldHotKeys $ \(key, state) -> do
    unless (key `elem` processedKeys) $ do
      let state' = updateState Nothing state
      liftIO $ setState state' key keys

  newHotKeys <- liftIO $ readIORef hotKeys
  insertRes $ HotKeys newHotKeys

  newKeys <- liftIO $ readIORef keys
  insertRes newKeys

updateState :: Maybe SDL3.SDL_EventType -> KeyState -> KeyState
updateState (Just SDL3.SDL_EVENT_KEY_DOWN) state = updateStateDown state
updateState (Just SDL3.SDL_EVENT_KEY_UP) state = updateStateUp state
updateState Nothing state = updateStateIdle state
updateState _ state = state

updateStateIdle :: KeyState -> KeyState
updateStateIdle Released = Released
updateStateIdle JustPressed = Pressed
updateStateIdle Pressed = Pressed
updateStateIdle JustReleased = Released

updateStateDown :: KeyState -> KeyState
updateStateDown Released = JustPressed
updateStateDown JustPressed = Pressed
updateStateDown Pressed = Pressed
updateStateDown JustReleased = JustPressed

updateStateUp :: KeyState -> KeyState
updateStateUp Released = Released
updateStateUp JustPressed = JustReleased
updateStateUp Pressed = JustReleased
updateStateUp JustReleased = Released