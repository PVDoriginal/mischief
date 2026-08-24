module Mischief.Render.Camera where

import Codec.Picture
import Data.Bits
import Data.Default
import Data.Primitive.Ptr (nullPtr)
import Data.Vector.Storable qualified as VS
import Data.Word
import Foreign.C.ConstPtr
import Mischief.ECS.Prelude
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data Camera = Camera

instance Component Camera where
  required = require @CameraOutput

data CameraOutput = CameraOutput deriving (Component)

instance Default CameraOutput where
  def = CameraOutput
