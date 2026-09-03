{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Mischief.Input.Keys
  ( -- * Resource
    Keys,

    -- * Functions
    pressed,
    released,
    justPressed,
    justReleased,

    -- * Plugin
    KeysPlugin (..),

    -- * Keys
    -- $keys
    pattern Mischief.Input.Keys.A,
    pattern Mischief.Input.Keys.B,
    pattern Mischief.Input.Keys.C,
    pattern Mischief.Input.Keys.D,
    pattern Mischief.Input.Keys.E,
    pattern Mischief.Input.Keys.F,
    pattern Mischief.Input.Keys.G,
    pattern Mischief.Input.Keys.H,
    pattern Mischief.Input.Keys.I,
    pattern Mischief.Input.Keys.J,
    pattern Mischief.Input.Keys.K,
    pattern Mischief.Input.Keys.L,
    pattern Mischief.Input.Keys.M,
    pattern Mischief.Input.Keys.N,
    pattern Mischief.Input.Keys.O,
    pattern Mischief.Input.Keys.P,
    pattern Mischief.Input.Keys.Q,
    pattern Mischief.Input.Keys.R,
    pattern Mischief.Input.Keys.S,
    pattern Mischief.Input.Keys.T,
    pattern Mischief.Input.Keys.U,
    pattern Mischief.Input.Keys.V,
    pattern Mischief.Input.Keys.W,
    pattern Mischief.Input.Keys.X,
    pattern Mischief.Input.Keys.Y,
    pattern Mischief.Input.Keys.Z,
    pattern Mischief.Input.Keys.Key0,
    pattern Mischief.Input.Keys.Key1,
    pattern Mischief.Input.Keys.Key2,
    pattern Mischief.Input.Keys.Key3,
    pattern Mischief.Input.Keys.Key4,
    pattern Mischief.Input.Keys.Key5,
    pattern Mischief.Input.Keys.Key6,
    pattern Mischief.Input.Keys.Key7,
    pattern Mischief.Input.Keys.Key8,
    pattern Mischief.Input.Keys.Key9,
    pattern Mischief.Input.Keys.Enter,
    pattern Mischief.Input.Keys.Escape,
    pattern Mischief.Input.Keys.Backspace,
    pattern Mischief.Input.Keys.Tab,
    pattern Mischief.Input.Keys.Space,
    pattern Mischief.Input.Keys.Minus,
    pattern Mischief.Input.Keys.Equals,
    pattern Mischief.Input.Keys.LeftBracket,
    pattern Mischief.Input.Keys.RightBracket,
    pattern Mischief.Input.Keys.BackSlash,
    pattern Mischief.Input.Keys.Semicolon,
    pattern Mischief.Input.Keys.Apostrophe,
    pattern Mischief.Input.Keys.Grave,
    pattern Mischief.Input.Keys.Comma,
    pattern Mischief.Input.Keys.Period,
    pattern Mischief.Input.Keys.Slash,
    pattern Mischief.Input.Keys.Capslock,
    pattern Mischief.Input.Keys.F1,
    pattern Mischief.Input.Keys.F2,
    pattern Mischief.Input.Keys.F3,
    pattern Mischief.Input.Keys.F4,
    pattern Mischief.Input.Keys.F5,
    pattern Mischief.Input.Keys.F6,
    pattern Mischief.Input.Keys.F7,
    pattern Mischief.Input.Keys.F8,
    pattern Mischief.Input.Keys.F9,
    pattern Mischief.Input.Keys.F10,
    pattern Mischief.Input.Keys.F11,
    pattern Mischief.Input.Keys.F12,
    pattern Mischief.Input.Keys.Printscreen,
    pattern Mischief.Input.Keys.Scrolllock,
    pattern Mischief.Input.Keys.Pause,
    pattern Mischief.Input.Keys.Insert,
    pattern Mischief.Input.Keys.Home,
    pattern Mischief.Input.Keys.PageUp,
    pattern Mischief.Input.Keys.PageDown,
    pattern Mischief.Input.Keys.Delete,
    pattern Mischief.Input.Keys.End,
    pattern Mischief.Input.Keys.Right,
    pattern Mischief.Input.Keys.Left,
    pattern Mischief.Input.Keys.Down,
    pattern Mischief.Input.Keys.Up,
    pattern Mischief.Input.Keys.Numlockclear,
    pattern Mischief.Input.Keys.KeypadDivide,
    pattern Mischief.Input.Keys.KeypadMultiply,
    pattern Mischief.Input.Keys.Keypad0,
    pattern Mischief.Input.Keys.Keypad1,
    pattern Mischief.Input.Keys.Keypad2,
    pattern Mischief.Input.Keys.Keypad3,
    pattern Mischief.Input.Keys.Keypad4,
    pattern Mischief.Input.Keys.Keypad5,
    pattern Mischief.Input.Keys.Keypad6,
    pattern Mischief.Input.Keys.Keypad7,
    pattern Mischief.Input.Keys.Keypad8,
    pattern Mischief.Input.Keys.Keypad9,
    pattern Mischief.Input.Keys.KeypadPeriod,
    pattern Mischief.Input.Keys.KeypadComma,
    pattern Mischief.Input.Keys.KeypadEnter,
    pattern Mischief.Input.Keys.KeypadEquals,
    pattern Mischief.Input.Keys.KeypadMinus,
    pattern Mischief.Input.Keys.KeypadPlus,
    pattern Mischief.Input.Keys.LCtrl,
    pattern Mischief.Input.Keys.LShift,
    pattern Mischief.Input.Keys.LAlt,
    pattern Mischief.Input.Keys.RCtrl,
    pattern Mischief.Input.Keys.RShift,
    pattern Mischief.Input.Keys.RAlt,
    pattern Mischief.Input.Keys.Application,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Foreign (toBool)
import GHC.Generics
import Mischief.ECS
import Mischief.ECS.Messages qualified as Messages
import Mischief.ECS.Systems qualified as Systems
import Mischief.SDL (SDLMessage (..), SDLPlugin (..))
import SDL3.Sys qualified as SDL3

-- | A resource that contains data on keyboard input.
data Keys = Keys
  { physical :: Map SDL3.SDL_Scancode KeyState,
    virtual :: Map SDL3.SDL_Keycode KeyState
  }
  deriving (Generic, Default, Component)

newtype HotKeys = HotKeys [((SDL3.SDL_Scancode, SDL3.SDL_Keycode), KeyState)] deriving (Generic, Default, Component)

-- | Check if a key is currently pressed.
pressed :: SDL3.SDL_Scancode -> Keys -> Bool
pressed scancode keys = case Map.lookup scancode keys.physical of
  Just Pressed -> True
  Just JustPressed -> True
  _ -> False

-- | Check if a key is currently not pressed.
released :: SDL3.SDL_Scancode -> Keys -> Bool
released a b = not $ pressed a b

-- | Check if a key has been pressed this frame.
justPressed :: SDL3.SDL_Scancode -> Keys -> Bool
justPressed scancode keys = case Map.lookup scancode keys.physical of
  Just JustPressed -> True
  _ -> False

-- | Check if a key has been released this frame.
justReleased :: SDL3.SDL_Scancode -> Keys -> Bool
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

-- setStatePhysical :: KeyState -> SDL3.SDL_Scancode -> IORef Keys -> IO ()
-- setStatePhysical state scancode keys =
--   modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical = Map.insert scancode state physical, virtual})

-- setStateVirtual :: KeyState -> SDL3.SDL_Keycode -> IORef Keys -> IO ()
-- setStateVirtual state keycode keys =
--   modifyIORef' keys (\Keys {physical, virtual} -> Keys {physical, virtual = Map.insert keycode state virtual})

data KeyState = Pressed | JustPressed | Released | JustReleased
  deriving (Show, Eq)

-- | Plugin which keeps track of key inputs and updates the 'Keys' resource.
data KeysPlugin = KeysPlugin deriving (Eq)

instance Plugin KeysPlugin where
  init _ = do
    insertRes (def @Keys)
    insertRes (def @HotKeys)
    Systems.add First readEvents

  plugins _ = plug SDLPlugin

readEvents :: System ()
readEvents = do
  messages <- Messages.read @(SDLMessage SDL3.SDL_KeyboardEvent)

  Just keys <- res @Keys

  keys <- liftIO $ newIORef keys
  hotKeys <- liftIO $ newIORef []
  processedKeys <- liftIO $ newIORef []

  for_ messages $ \(SDLMessage {eventType, event}) -> do
    unless (toBool event.repeat) $ do
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

pattern A = SDL3.SDL_SCANCODE_A

pattern B = SDL3.SDL_SCANCODE_B

pattern C = SDL3.SDL_SCANCODE_C

pattern D = SDL3.SDL_SCANCODE_D

pattern E = SDL3.SDL_SCANCODE_E

pattern F = SDL3.SDL_SCANCODE_F

pattern G = SDL3.SDL_SCANCODE_G

pattern H = SDL3.SDL_SCANCODE_H

pattern I = SDL3.SDL_SCANCODE_I

pattern J = SDL3.SDL_SCANCODE_J

pattern K = SDL3.SDL_SCANCODE_K

pattern L = SDL3.SDL_SCANCODE_L

pattern M = SDL3.SDL_SCANCODE_M

pattern N = SDL3.SDL_SCANCODE_N

pattern O = SDL3.SDL_SCANCODE_O

pattern P = SDL3.SDL_SCANCODE_P

pattern Q = SDL3.SDL_SCANCODE_Q

pattern R = SDL3.SDL_SCANCODE_R

pattern S = SDL3.SDL_SCANCODE_S

pattern T = SDL3.SDL_SCANCODE_T

pattern U = SDL3.SDL_SCANCODE_U

pattern V = SDL3.SDL_SCANCODE_V

pattern W = SDL3.SDL_SCANCODE_W

pattern X = SDL3.SDL_SCANCODE_X

pattern Y = SDL3.SDL_SCANCODE_Y

pattern Z = SDL3.SDL_SCANCODE_Z

pattern Key1 = SDL3.SDL_SCANCODE_1

pattern Key2 = SDL3.SDL_SCANCODE_2

pattern Key3 = SDL3.SDL_SCANCODE_3

pattern Key4 = SDL3.SDL_SCANCODE_4

pattern Key5 = SDL3.SDL_SCANCODE_5

pattern Key6 = SDL3.SDL_SCANCODE_6

pattern Key7 = SDL3.SDL_SCANCODE_7

pattern Key8 = SDL3.SDL_SCANCODE_8

pattern Key9 = SDL3.SDL_SCANCODE_9

pattern Key0 = SDL3.SDL_SCANCODE_0

pattern Enter = SDL3.SDL_SCANCODE_RETURN

pattern Escape = SDL3.SDL_SCANCODE_ESCAPE

pattern Backspace = SDL3.SDL_SCANCODE_BACKSPACE

pattern Tab = SDL3.SDL_SCANCODE_TAB

pattern Space = SDL3.SDL_SCANCODE_SPACE

pattern Minus = SDL3.SDL_SCANCODE_MINUS

pattern Equals = SDL3.SDL_SCANCODE_EQUALS

pattern LeftBracket = SDL3.SDL_SCANCODE_LEFTBRACKET

pattern RightBracket = SDL3.SDL_SCANCODE_RIGHTBRACKET

pattern BackSlash = SDL3.SDL_SCANCODE_BACKSLASH

pattern Semicolon = SDL3.SDL_SCANCODE_SEMICOLON

pattern Apostrophe = SDL3.SDL_SCANCODE_APOSTROPHE

pattern Grave = SDL3.SDL_SCANCODE_GRAVE

pattern Comma = SDL3.SDL_SCANCODE_COMMA

pattern Period = SDL3.SDL_SCANCODE_PERIOD

pattern Slash = SDL3.SDL_SCANCODE_SLASH

pattern Capslock = SDL3.SDL_SCANCODE_CAPSLOCK

pattern F1 = SDL3.SDL_SCANCODE_F1

pattern F2 = SDL3.SDL_SCANCODE_F2

pattern F3 = SDL3.SDL_SCANCODE_F3

pattern F4 = SDL3.SDL_SCANCODE_F4

pattern F5 = SDL3.SDL_SCANCODE_F5

pattern F6 = SDL3.SDL_SCANCODE_F6

pattern F7 = SDL3.SDL_SCANCODE_F7

pattern F8 = SDL3.SDL_SCANCODE_F8

pattern F9 = SDL3.SDL_SCANCODE_F9

pattern F10 = SDL3.SDL_SCANCODE_F10

pattern F11 = SDL3.SDL_SCANCODE_F11

pattern F12 = SDL3.SDL_SCANCODE_F12

pattern Printscreen = SDL3.SDL_SCANCODE_PRINTSCREEN

pattern Scrolllock = SDL3.SDL_SCANCODE_SCROLLLOCK

pattern Pause = SDL3.SDL_SCANCODE_PAUSE

pattern Insert = SDL3.SDL_SCANCODE_INSERT

pattern Home = SDL3.SDL_SCANCODE_HOME

pattern PageUp = SDL3.SDL_SCANCODE_PAGEUP

pattern Delete = SDL3.SDL_SCANCODE_DELETE

pattern End = SDL3.SDL_SCANCODE_END

pattern PageDown = SDL3.SDL_SCANCODE_PAGEDOWN

pattern Right = SDL3.SDL_SCANCODE_RIGHT

pattern Left = SDL3.SDL_SCANCODE_LEFT

pattern Down = SDL3.SDL_SCANCODE_DOWN

pattern Up = SDL3.SDL_SCANCODE_UP

pattern Numlockclear = SDL3.SDL_SCANCODE_NUMLOCKCLEAR

pattern KeypadDivide = SDL3.SDL_SCANCODE_KP_DIVIDE

pattern KeypadMultiply = SDL3.SDL_SCANCODE_KP_MULTIPLY

pattern KeypadMinus = SDL3.SDL_SCANCODE_KP_MINUS

pattern KeypadPlus = SDL3.SDL_SCANCODE_KP_PLUS

pattern KeypadEnter = SDL3.SDL_SCANCODE_KP_ENTER

pattern Keypad1 = SDL3.SDL_SCANCODE_KP_1

pattern Keypad2 = SDL3.SDL_SCANCODE_KP_2

pattern Keypad3 = SDL3.SDL_SCANCODE_KP_3

pattern Keypad4 = SDL3.SDL_SCANCODE_KP_4

pattern Keypad5 = SDL3.SDL_SCANCODE_KP_5

pattern Keypad6 = SDL3.SDL_SCANCODE_KP_6

pattern Keypad7 = SDL3.SDL_SCANCODE_KP_7

pattern Keypad8 = SDL3.SDL_SCANCODE_KP_8

pattern Keypad9 = SDL3.SDL_SCANCODE_KP_9

pattern Keypad0 = SDL3.SDL_SCANCODE_KP_0

pattern KeypadPeriod = SDL3.SDL_SCANCODE_KP_PERIOD

pattern KeypadEquals = SDL3.SDL_SCANCODE_KP_EQUALS

pattern KeypadComma = SDL3.SDL_SCANCODE_KP_COMMA

pattern LCtrl = SDL3.SDL_SCANCODE_LCTRL

pattern LShift = SDL3.SDL_SCANCODE_LSHIFT

pattern LAlt = SDL3.SDL_SCANCODE_LALT

pattern RCtrl = SDL3.SDL_SCANCODE_RCTRL

pattern RShift = SDL3.SDL_SCANCODE_RSHIFT

pattern RAlt = SDL3.SDL_SCANCODE_RALT

pattern Application = SDL3.SDL_SCANCODE_APPLICATION

-- $keys
-- Below is an opinionated collection of keys selected from SDL3's scancodes. These can be used with 'pressed', 'justPressed', and so on.
--
-- Most key names are the same as SDL's, the only significant exception is @Return@ which has been renamed to @Enter@.
--
-- If you wish to access a key that's not defined here, you can grab it directly from "SDL3.Sys.Bindgen.Scancode".