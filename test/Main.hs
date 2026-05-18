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
  e2 <- spawn (C1 5 2) world    
  e3 <- spawn (C1 1 3) world        
  e4 <- spawn (C2 "lmao", C1 1 1) world    
  e5 <- spawn (C1 5 9, C2 "lol") world    
  e6 <- spawn (C2 "haha", C3 5.3) world
  e7 <- spawn (C1 9 9, C2 "ugh", C3 4.1) world
  e8 <- spawn (C3 9.4, C2 "eh") world

  e9 <- spawn () world 

  insert (C1 100 100, C2 "new archetype!") e1 world 

  remove (type C1) e1 world 

  despawn e1 world  

  putStrLn "\n"

  c <- query @(C2) world
  putStrLn $ show c 