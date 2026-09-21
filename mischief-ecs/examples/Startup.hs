module Main where

import Control.Monad
import Data.Text qualified as T
import Mischief.ECS (ChildOf (ChildOf))
import Mischief.ECS.Prelude
import Mischief.ECS.Relationships.Graph qualified as Graph
import Mischief.ECS.World.Query

main :: IO ()
main = do
  app <- newApp
  addPlugin @MyPlugin app
  runApp app

data MyPlugin

instance Plugin MyPlugin where
  init :: System ()
  init = do
    systems (helloWorld, greetPeople, showLikes)
      & schedule Update

    systems addPeople
      & schedule Startup

    systems updateFlo
      & before greetPeople
      & schedule Update

    insertRes (Greeting "Hey")

data Person = Person deriving (Component)

addPeople :: System ()
addPeople =
  do
    kim <- spawn (Person, Name "Kimberly")
    nick <- spawn (Person, Name "Nicholas")
    flo <- spawn (Person, Name "Florian")

    insert (Rel Likes kim) flo
    insert (Rel Likes nick, Rel Likes flo) kim

helloWorld :: System ()
helloWorld = info "Hello World!"

newtype Pos = Pos Int deriving (Component, Show)

newtype Vel = Vel Int deriving (Component, Show)

data Greeting = Greeting String deriving (Component)

instance Show Greeting where
  show (Greeting a) = a

data Likes = Likes deriving (Component)

greetPeople :: System ()
greetPeople = do
  [q|Name, Res Greeting / With Person|]
    & qinfo (\(name, greeting) -> [i|#{greeting} #{name}!|])
    & query_

updateFlo :: System ()
updateFlo = do
  [q|Name|]
    & qfilter (== Name "Florian")
    & qinsert (\_ -> Name "Florianne")
    & query_

showLikes :: System ()
showLikes = do
  [q|Name|]
    & qrelateMany (Graph.outgoing @Likes) [qd|Name|] (,)
    & qinfo (\(name, likes) -> [i|#{name} likes #{map (.comp) likes}|])
    & query_
