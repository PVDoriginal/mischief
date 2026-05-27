module Main where

import Control.Monad.IO.Class
import Data.Foldable hiding (and, or)
import Data.List qualified as List
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Mischief
import MischiefInput (inputPlugin)

newtype Counter = Counter Int deriving (Component, Queryable, Show)

newtype C1 = C1 String deriving (Component, Queryable, Show)

newtype C2 = C2 String deriving (Component, Queryable, Show)

newtype C3 = C3 String deriving (Component, Queryable, Show)

newtype Res = Res String deriving (Queryable, Show)

instance Component Res where
  type Storage Res = ResourceStorage

main :: IO ()
main = do
  app <- newApp [inputPlugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystem Startup setup
  addSystem Update update

setup :: System ()
setup = do
  insertResource (Res "Resource")

  return ()

update :: System ()
update = do
  res <- query' @(Entity, (Name, Res)) $ NoFilter

  for_ res $ \(entity, (name, res)) -> do
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name ++ " and res: " ++ show res

    modify res (\_ -> Res "Lol")