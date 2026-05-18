module Main where

import MischiefECS

data C1 = C1 Int Int deriving (Component, Show) 
data C2 = C2 String deriving (Component, Show) 
data C3 = C3 Double deriving (Component, Show) 
data C4 = C4 Int String deriving (Component, Show) 


main :: IO ()
main = do 
  world <- newWorld
  
  e1 <- spawnEntity (C1 2 3) world    
  e2 <- spawnEntity (C1 5 2) world    
  e3 <- spawnEntity (C1 1 3) world        
  e4 <- spawnEntity (C2 "lmao", C1 1 1) world    
  e5 <- spawnEntity (C1 5 9, C2 "lol") world    
  e6 <- spawnEntity (C2 "haha", C3 5.3) world
  e7 <- spawnEntity (C1 9 9, C2 "ugh", C3 4.1) world
  e8 <- spawnEntity (C3 9.4, C2 "eh") world

  e9 <- spawnEntity () world 

  insertComponents (C1 100 100) e1 world 

  putStrLn "\n"

  c <- query (type (C1)) world
  putStrLn $ show c 