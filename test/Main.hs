{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import MyLib
import Data.IORef (readIORef)

main :: IO ()
main = do 
    world <- newWorld  
    incrementEntityCounter world
    counter <- readIORef world.entityCounter 
    putStrLn $ show counter 
