import Mischief.ECS
import Mischief.ECS.App
import Mischief.ECS.SDL
import MischiefAssets
import MischiefAssets.Asset
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