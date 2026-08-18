module Mischief.Input where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (def))
import Data.Map (Map)
import GHC.Generics (Generic)
import Mischief.ECS.Prelude
import Mischief.Input.Keys (KeysPlugin (..))

data InputPlugin = InputPlugin deriving (Eq)

instance Plugin InputPlugin where
  plugins _ = plug KeysPlugin