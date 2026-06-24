import MischiefECS.App
import MischiefECS.SDL
import MischiefRender

main :: IO ()
main = do
  app <- newApp [sdlPlugin, renderPlugin]
  runApp app
