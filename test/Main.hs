module Main where

import MischiefECS
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

data C1 = C1 Int Int deriving (Component, Show) 
data C2 = C2 String deriving (Component, Show) 
data C3 = C3 Double deriving (Component, Show) 
data C4 = C4 Int String deriving (Component, Show) 


main :: IO ()
main = do 
  world <- newWorld
  runReaderT system world   

system :: System () 
system = do 
  e1 <- spawn (C1 5 5, C2 "Lmfao") 
  insert (C3 5.3) e1
  
  res1 <- query @(C2, C3)     
  liftIO $ putStrLn $ show res1  

  remove (type C2) e1 

  res2 <- query @(C2, C3)     
  liftIO $ putStrLn $ show res2 

  res3 <- query @C1     
  liftIO $ putStrLn $ show res3 

  despawn e1 

  res4 <- query @C1     
  liftIO $ putStrLn $ show res4 

