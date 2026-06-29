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

loadShader :: ShaderType -> ByteString -> IO Shader
loadShader shaderType bytes = do
  let t = case shaderType of
        ShaderV -> "vertex"
        ShaderF -> "fragment"

  withSystemTempFile "original.hlsl" $ \originalPath originalH -> do
    hClose originalH
    B.writeFile originalPath bytes

    spv <- withSystemTempFile "new.spv" $ \newPath newH -> do
      callProcess
        shadercross
        [originalPath, "-e", "main", "-t", t, "-o", newPath]

      hClose newH
      B.readFile newPath

    return $ Shader {original = bytes, spv}

newtype VertexShader = VertexShader {inner :: Shader}

instance Asset VertexShader where
  loadAsset :: FilePath -> IO VertexShader
  loadAsset b = do
    bytes <- B.readFile b
    shader <- loadShader ShaderV bytes
    return $ VertexShader shader

  extensions :: [String]
  extensions = ["vert.hlsl"]

newtype FragmentShader = FragmentShader {inner :: Shader}

instance Asset FragmentShader where
  loadAsset :: FilePath -> IO FragmentShader
  loadAsset b = do
    bytes <- B.readFile b
    shader <- loadShader ShaderF bytes
    return $ FragmentShader shader

  extensions :: [String]
  extensions = ["frag.hlsl"]