import MischiefAssets
import MischiefAssets.Asset
import MischiefECS
import MischiefECS.App
import MischiefECS.SDL
import MischiefRender

main :: IO ()
main = do
  app <- newApp [assetPlugin, sdlPlugin, renderPlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup s

s :: System ()
s = do
  _ <- load "assets/test.frag.hlsl"
  _ <- load "assets/test.vert.hlsl"
  return ()