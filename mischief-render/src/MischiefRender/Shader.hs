module MischiefRender.Shader where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import GHC.IO.Handle (hClose)
import MischiefAssets
import System.IO.Temp
import System.Process

shadercross :: String
shadercross = "/home/pvd/SDL_shadercross/build/shadercross"

data ShaderType = ShaderV | ShaderF

data Shader = Shader
  { original :: ByteString,
    spv :: ByteString
  }

loadShader :: ShaderType -> FilePath -> IO Shader
loadShader !shaderType !path = do
  let t = case shaderType of
        ShaderV -> "vertex"
        ShaderF -> "fragment"

  original <- B.readFile path

  spv <- withSystemTempFile "new.spv" $ \newPath newH -> do
    callProcess
      shadercross
      [path, "-e", "main", "-t", t, "-o", newPath]

    hClose newH
    B.readFile newPath

  return $ Shader {original, spv}

newtype VertexShader = VertexShader {inner :: Shader}

instance Asset VertexShader where
  loadAsset :: FilePath -> IO VertexShader
  loadAsset !path = do
    shader <- loadShader ShaderV path
    return $ VertexShader shader

  extensions :: [String]
  extensions = ["vert.hlsl"]

newtype FragmentShader = FragmentShader {inner :: Shader}

instance Asset FragmentShader where
  loadAsset :: FilePath -> IO FragmentShader
  loadAsset !path = do
    shader <- loadShader ShaderF path
    return $ FragmentShader shader

  extensions :: [String]
  extensions = ["frag.hlsl"]