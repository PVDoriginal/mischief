module Main where

import MischiefECS
import Data.IORef (readIORef)
import Data.Typeable

data C1 = C1 deriving Component 
data C2 = C2 deriving Component 
data C3 = C3 deriving Component 
data C4 = C4 deriving Component 


main :: IO ()
main = do 
    world <- newWorld
    spawnEntity (C1) world    
    spawnEntity (C1) world    
    spawnEntity (C1) world        
    spawnEntity (C2, C1) world    
    spawnEntity (C1, C2) world    
    spawnEntity (C2, C3) world
    spawnEntity (C1, C2, C3) world
    spawnEntity (C3, C2) world
    putStrLn "\n"
