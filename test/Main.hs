module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Data (typeOf)
import MischiefECS

data C1 = C1 Int Int deriving (Component, Show)

data C2 = C2 String deriving (Component, Show)

data C3 = C3 Double deriving (Component, Show)

data C4 = C4 Int String deriving (Component, Show)

main :: IO ()
main = do
  app <- newApp [plugin1]
  runApp app

plugin1 :: Plugin ()
plugin1 = do
  addSystem system1
  addSystem system2
  addPlugin plugin2

plugin2 :: Plugin ()
plugin2 = do
  addSystem system1

system1 :: System ()
system1 = do
  e1 <- spawn (C1 5 5, C2 "Lmfao")
  insert (C3 5.3) e1

  res2 <- query @(C2, C3)
  liftIO $ print res2

  res2 <- query @(Entity, C3)
  liftIO $ print res2

system2 :: System ()
system2 = do
  res2 <- query @(Entity, C3)
  liftIO $ print res2

-- res3 <- query @C1
-- liftIO $ print res3

-- res4 <- query @C1
-- liftIO $ print res4
