module Main where

import Control.Monad.IO.Class
import Data.Foldable
import Data.List qualified as List
import Data.Set qualified as Set
import GHC.Generics (Generic)
import MischiefECS.Prelude

newtype Counter = Counter Int deriving (Component, Queryable, Show)

data C1 = C1 String deriving (Component, Queryable, Show)

data C2 = C2 String deriving (Component, Queryable, Show)

data C3 = C3 String deriving (Component, Queryable, Show)

main :: IO ()
main = do
  app <- newApp [test2]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystem Startup setup
  addSystem Update update

setup :: System ()
setup = do
  spawn (Counter 0)
  spawn (Name "Lol", Counter $ -100000)

  return ()

update :: System ()
update = do
  res <- query @(Entity, (Counter, Name))

  for_ res $ \(entity, (counter, name)) -> do
    modify counter (\(Counter x) -> Counter $ x + 1)
    liftIO $ putStrLn $ show entity ++ " has name: " ++ show name ++ " and counter: " ++ show counter

test1 :: Plugin ()
test1 = do
  addSystem Startup $ do
    for_ [1 .. 10] $ \i -> spawn (Name "A", Counter i)
    for_ [1 .. 20] $ \i -> spawn (Name "B", Counter i)
    for_ [1 .. 30] $ \i -> spawn (Name "C", Counter i)

    query <- query @(Entity, Name)
    for_ query $ \(entity, name) ->
      case name.value of
        Name "A" -> insert (C1 "component 1") entity
        Name "B" -> insert (C2 "component 2") entity
        Name "C" -> insert (C3 "component 3") entity

  addSystem Update $ do
    query1 <- query @(Name, C1)
    for_ query1 $ \(name, c) ->
      liftIO $ putStrLn $ show name ++ " has " ++ show c

    query2 <- query @(Name, C2)
    for_ query2 $ \(name, c) ->
      liftIO $ putStrLn $ show name ++ " has " ++ show c

    query2 <- query @(Name, C3)
    for_ query2 $ \(name, c) ->
      liftIO $ putStrLn $ show name ++ " has " ++ show c

test2 :: Plugin ()
test2 = do
  addSystem Startup $ do
    e1 <- spawn (Name "A", (C2 "Component 2 on A", C3 "Component 3 on A"))
    e2 <- spawn (Name "B", (C1 "Component 1 on B", C3 "Component 3 on B"))
    e3 <- spawn (Name "C", (C1 "Component 1 on C", C2 "Component 2 on C"))

    remove @(C2, (C1, C3)) e1
    insert (C1 "New Component 1 on A", C2 "New Component 2 on A") e1

    remove @C2 e3
    insert (C2 "New Component 2 on C") e3

    remove @C3 e2

    query' <- query @C1
    liftIO $ putStrLn $ "C1 query result: " ++ show query'

    query' <- query @(C1, C2)
    liftIO $ putStrLn $ "(C1, C2) query result: " ++ show query'

    query' <- query @(C1, (C2, C3))
    liftIO $ putStrLn $ "(C1, (C2, C3)) query result: " ++ show query'

    query' <- query @C2
    liftIO $ putStrLn $ "C2 query result: " ++ show query'

    query' <- query @C3
    liftIO $ putStrLn $ "C3 query result: " ++ show query'

    return ()