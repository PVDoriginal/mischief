module Mischief.Render.Bind where

import Control.Monad.IO.Class
import Data.Primitive.Ptr
import Foreign.C.ConstPtr
import Mischief.ECS.Prelude
import Mischief.Render.Core (Sampler (..))
import Mischief.Render.Shader.Bindings
import Mischief.Render.Texture
import Mischief.WGPU
import Mischief.WGPU.Types.General

-- bindTexture :: Texture -> System (Binding index Texture)
-- bindTexture (Texture {texture}) = do
--   textureView <- liftIO $ wgpuTextureCreateView texture (ConstPtr nullPtr)
--   pure $
--     BindEntry $
--       WGPUBindGroupEntry
--         { binding = 0,
--           textureView = textureView,
--           size = 0,
--           offset = 0,
--           sampler = nullPtr,
--           buffer = nullPtr,
--           nextInChain = nullPtr
--         }

-- bindSampler :: Sampler -> System (Binding index Sampler)
-- bindSampler (Sampler sampler) =
--   pure $
--     BindEntry $
--       WGPUBindGroupEntry
--         { binding = 1,
--           textureView = nullPtr,
--           size = 0,
--           offset = 0,
--           sampler = sampler,
--           buffer = nullPtr,
--           nextInChain = nullPtr
--         }
