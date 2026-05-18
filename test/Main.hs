module Main where

import MischiefECS

data C1 = C1 Int Int deriving (Component, Show) 
data C2 = C2 String deriving (Component, Show) 
data C3 = C3 Double deriving (Component, Show) 
data C4 = C4 Int String deriving (Component, Show) 


main :: IO ()
main = do 
  world <- newWorld
  
  e1 <- spawn (C1 2 100) world
  despawn e1 world  

  putStrLn "\n"

  c <- query @(C1) world
  putStrLn $ show c 