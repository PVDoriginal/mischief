module MischiefECS.Log where

import System.Console.ANSI

logMsg :: Color -> String -> IO ()
logMsg c msg = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn msg
  setSGR [Reset]

info :: String -> IO ()
info = print

warn :: String -> IO ()
warn m = logMsg Yellow $ "Warning: " ++ m

error :: String -> IO ()
error m = logMsg Red $ "Warning: " ++ m
