module MischiefInput.Keyboard where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import MischiefECS
import MischiefECS.App.Scheduler
import SDL3

data Keys = Keys
  { physical :: Map SDLScancode KeyState,
    virtual :: Map SDLKeycode KeyState
  }
  deriving (Generic, Queryable, Default)

newtype HotKeys = HotKeys [((SDLScancode, SDLKeycode), KeyState)] deriving (Generic, Queryable, Default)

instance Component HotKeys where
  type Storage HotKeys = ResourceStorage

pressed :: SDLScancode -> Result Keys -> Bool
pressed scancode keys = case Map.lookup scancode keys.value.physical of
  Just Pressed -> True
  Just JustPressed -> True
  _ -> False

justPressed :: SDLScancode -> Result Keys -> Bool
justPressed scancode keys = case Map.lookup scancode keys.value.physical of
  Just JustPressed -> True
  _ -> False

justReleased :: SDLScancode -> Result Keys -> Bool
justReleased scancode keys = case Map.lookup scancode keys.value.physical of
  Just JustReleased -> True
  _ -> False

getState :: SDLScancode -> IORef Keys -> IO KeyState
getState scancode keys' = do
  keys <- readIORef keys'
  case Map.lookup scancode keys.physical of
    Just x -> return x
    Nothing -> do
      modifyIORef' keys' (\Keys {physical, virtual} -> Keys {physical = Map.insert scancode Released physical, virtual})
      return Released

setState :: KeyState -> (SDLScancode, SDLKeycode) -> IORef Keys -> IO ()
setState state (scancode, keycode) keys =
  modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical = Map.insert scancode state physical, virtual = Map.insert keycode state virtual})

setStatePhysical :: KeyState -> SDLScancode -> IORef Keys -> IO ()
setStatePhysical state scancode keys =
  modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical = Map.insert scancode state physical, virtual})

setStateVirtual :: KeyState -> SDLKeycode -> IORef Keys -> IO ()
setStateVirtual state keycode keys =
  modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical, virtual = Map.insert keycode state virtual})

instance Component Keys where
  type Storage Keys = ResourceStorage

data KeyState = Pressed | JustPressed | Released | JustReleased
  deriving (Show, Eq)

keyboardPlugin :: Plugin ()
keyboardPlugin = do
  addResource (def @Keys)
  addResource (def @HotKeys)
  addSystems Update readEvents

readEvents :: System ()
readEvents = do
  Just msg <- single @(Messages (SDLMessage SDLKeyboardEvent))
  messages <- readMessages msg

  Just keys' <- single @Keys

  keys <- liftIO $ newIORef keys'.value
  hotKeys <- liftIO $ newIORef []
  processedKeys <- liftIO $ newIORef []

  for_ messages $ \(SDLMessage event) -> do
    state <- liftIO $ getState event.sdlKeyboardScancode keys
    let state' = updateState (Just event.sdlKeyboardType) state
    liftIO $ setState state' (event.sdlKeyboardScancode, event.sdlKeyboardKey) keys

    when (state' == JustReleased || state' == JustPressed) $
      liftIO $
        modifyIORef' hotKeys (++ [((event.sdlKeyboardScancode, event.sdlKeyboardKey), state')])

    liftIO $ modifyIORef' processedKeys (++ [(event.sdlKeyboardScancode, event.sdlKeyboardKey)])

  Just oldHotKeys' <- single @HotKeys
  let HotKeys oldHotKeys = oldHotKeys'.value
  processedKeys <- liftIO $ readIORef processedKeys

  for_ oldHotKeys $ \(key, state) -> do
    unless (key `elem` processedKeys) $ do
      let state' = updateState Nothing state
      liftIO $ setState state' key keys

  newHotKeys <- liftIO $ readIORef hotKeys
  insertResource $ HotKeys newHotKeys

  newKeys <- liftIO $ readIORef keys
  insertResource newKeys

updateState :: Maybe SDLEventType -> KeyState -> KeyState
updateState (Just SDL_EVENT_KEY_DOWN) state = updateStateDown state
updateState (Just SDL_EVENT_KEY_UP) state = updateStateUp state
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