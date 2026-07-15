module MischiefInput where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.Map (Map)
import GHC.Generics (Generic)
import Mischief.ECS
import MischiefInput.Keyboard (Keys (Keys), keyboardPlugin, pressed)
import SDL3
