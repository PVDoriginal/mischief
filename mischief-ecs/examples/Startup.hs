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

data Comp1 = Comp1 deriving (Component)

data Comp2 = Comp2 deriving (Component)

addPeople :: System ()
addPeople =
  do
    kim <- spawn (Person, Name "Kimberly")
    nick <- spawn (Person, Name "Nicholas")
    flo <- spawn (Person, Name "Florian")

    insert (Rel Likes kim) flo
    insert (Rel Likes nick, Rel Likes flo) kim

    void $ spawn Comp1
    void $ spawn Comp1
    void $ spawn Comp1
    void $ spawn Comp1

    void $ spawn Comp2
    void $ spawn Comp2
    void $ spawn Comp2
    void $ spawn Comp2
    void $ spawn Comp2

    [q|E / With Comp1|]
      & qinfo T.show
      & query_

    query_ $ do
      name <- [q|E / With Comp1|]
      [q|E, Name / With Comp2|]
        & qcollect name
        & qmap (name,)
        & qinfo T.show

    -- undefined
    pure ()

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
    & qinfo (\(name, likes) -> [i|#{name} likes #{likes}|])
    & query_

extraFrom :: System ()
extraFrom = do
  [q|Name|]
    & qrelateMany (Graph.incoming @ChildOf) [qd|Name|] (,)
    & qinsert (\(parentName, childNames) -> map (fmap . const $ parentName) childNames)
    & query_