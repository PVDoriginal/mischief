module Main where

import Mischief.ECS.Prelude

main :: IO ()
main = do
  app <- newApp
  addPlugin @MyPlugin app
  runApp app

data MyPlugin

instance Plugin MyPlugin where
  init = do
    systems (helloWorld, greetPeople, showLikes)
      & schedule @Update

    systems addPeople
      & schedule @Startup

    systems updateFlo
      & before (greetPeople, showLikes)
      & schedule @Update

    insertRes (Greeting "Hey")

helloWorld :: System ()
helloWorld = info "Hello World!"

data Person = Person deriving (Component)

addPeople :: System ()
addPeople = do
  kim <- spawn (Person, Name "Kimberly")
  nick <- spawn (Person, Name "Nicholas")
  flo <- spawn (Person, Name "Florian")

  insert (Rel Likes kim) flo
  insert (Rel Likes nick, Rel Likes flo) kim

greetPeople :: System ()
greetPeople = do
  Just greeting <- res @Greeting

  [q|Name / With Person|]
    & qinfo (\name -> [i|#{greeting} #{name}!|])
    & query_

updateFlo :: System ()
updateFlo = do
  [q|Name / With Person|]
    & qfilter (== Name "Florian")
    & qinsert (\_ -> Name "Florianne")
    & query_

data Greeting = Greeting String deriving (Component)

instance Show Greeting where
  show (Greeting a) = a

data Likes = Likes deriving (Component)

showLikes :: System ()
showLikes = do
  [q|Name, Likes -> (Name)|]
    & qinfo (\(name, likes) -> [i|#{name} likes #{likes}|])
    & query_
