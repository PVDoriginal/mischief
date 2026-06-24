module MischiefRender where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GPUCommon (Context (contextDevice))
import MischiefECS
import SDL3

renderPlugin :: Plugin ()
renderPlugin = do
  addSystems Startup setup

setup :: System ()
setup = do
  Just window <- single @Window
  Just device <- liftIO $ sdlCreateGPUDevice SDL_GPU_SHADERFORMAT_SPIRV True Nothing
  b <- liftIO $ sdlClaimWindowForGPUDevice device window.value.sdlWindow
  -- sdlUploadToGPUTexture

  liftIO $ print b
  return ()