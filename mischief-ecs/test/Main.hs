module Main where

import Mischief.ECS.Prelude
import Mischief.ECS.Relationships.Graph qualified as Graph

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
addPeople = do
  kim <- spawn (Person, Name "Kimberly")
  nick <- spawn (Person, Name "Nicholas")
  flo <- spawn (Person, Name "Florian")

  insert (Rel Likes kim) flo
  insert (Rel Likes nick, Rel Likes flo) kim

helloWorld :: System ()
helloWorld = info "Hello World!"

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
    & qmap (\_ -> Name "Florianne")
    & query_

showLikes :: System ()
showLikes = do
  [q|Name|]
    & qrelateMany (Graph.outgoing @Likes) (,) [q|Name|]
    & qinfo (\(name, likes) -> [i|#{name} likes #{likes}|])
    & query_

-- people <- query [q|E, Name / With Person|]
-- for_ people $ \(entity, name) -> do
--   when (name == Name "Florian") $
--     insert (Name "Florianne") entity

data Comp1 = Comp1 Int deriving (Component, Show)

data Position = Position Int deriving (Component, Show)

data Velocity = Velocity Int deriving (Component, Show)

-- instance Plugin MainPlugin where
--   init = do
--     -- a <- spawn (Name "A", Comp1 5, Position 2)
--     -- b <- spawn (Name "B", Velocity 3, Rel ChildOf a)

--     -- replicateM_ 10 $ qrun $ qmap (\(Velocity x, From a (Position y)) -> From a (Position $ x + y)) [q|Velocity, ChildOf -> (Position)|]
--     -- warn . text =<< get a [q|Name, Comp1, Position|]

--     a <- spawn (Name "A")
--     b <- spawn (Name "B", Rel ChildOf a)
--     c <- spawn (Name "C", Rel ChildOf a)
--     d <- spawn (Name "D", Rel ChildOf b)
--     e <- spawn (Name "E", Rel ChildOf b)
--     f <- spawn (Name "F", Rel ChildOf c)

--     g <- spawn (Name "G")
--     h <- spawn (Name "H", Rel ChildOf g)
--     i <- spawn (Name "I", Rel ChildOf g)

--     [q||]
--       & qrelateOne (Tree.root @ChildOf) (const id) [q|Name|]
--       & qinfo (("Root: " <>) . text)
--       & get_ i

--     [q||]
--       & qrelateMany (Tree.leaves @ChildOf) (const id) [q|Name|]
--       & qinfo (("Leaves: " <>) . text)
--       & get_ b

--     [q|Name|]
--       & qrelateOne ChildOf.parent (,) [q|Name|]
--       & qinfo (\(child, parent) -> text child <> " is child of " <> text parent)
--       & query_

--   deps = [dep @TimePlugin]
