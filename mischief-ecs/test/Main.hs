module Main where

import Data.Foldable
import Mischief.ECS.Prelude
import Mischief.ECS.Relationships.ChildOf
import Mischief.ECS.Relationships.Graph qualified as Graph
import Utils

main :: IO ()
main = do
  runTests
    [ test1,
      test2,
      test3,
      test4,
      test5
    ]

test1 :: System ()
test1 = do
  a <- spawn (Name "A", Comp1)
  b <- spawn (Name "B", Comp1)
  c <- spawn (Name "C", Comp1, Comp2)
  d <- spawn (Name "D", Comp2)
  e <- spawn (Name "E", Comp2)
  f <- spawn (Name "F", Comp1, Comp3)
  g <- spawn (Name "G", Comp2, Comp3)

  assertqWith
    [q|Name / With Comp1|]
    [(a, Name "A"), (b, Name "B"), (c, Name "C"), (f, Name "F")]

  assertqWith
    [q|Name / With Comp2|]
    [(c, Name "C"), (d, Name "D"), (e, Name "E"), (g, Name "G")]

  assertqWith
    [q|Name / With Comp3|]
    [(f, Name "F"), (g, Name "G")]

  assertqWith
    [q|Name / With Comp3, Without Comp2|]
    [(f, Name "F")]

  assertqWith
    [q|Name / With Comp1, With Comp2|]
    [(c, Name "C")]

test2 :: System ()
test2 = do
  a <- spawn (Pos 3, Velocity 2, Comp1)
  b <- spawn (Pos 4, Velocity 2, Comp2, Comp3)
  c <- spawn (Pos 5, Velocity 3)

  [q|Pos, Velocity / With Comp1 || (With Comp2, With Comp3)|]
    & qinsert (\(Pos p, Velocity v) -> Pos $ p + v)
    & query_

  assertqWith
    [q|Pos|]
    [(a, Pos 5), (b, Pos 6), (c, Pos 5)]

test3 :: System ()
test3 = do
  a <- spawn (Pos 3, Velocity 5)
  insert (Rel (Likes 1) a) a
  b <- spawn (Pos 4, Velocity 2, Rel (Likes 5) a)
  c <- spawn (Pos 2, Velocity 1, Rel (Likes 1) a, Rel (Likes 2) b)
  d <- spawn (Pos 3, Rel (Likes 2) a, Rel (Likes 3) c)

  query_ $ do
    (e, Pos p, r) <- [q|Entity, Pos, Likes -> *|]

    [q|Entity, Velocity|]
      & qmapMaybe (\(entity, v) -> fmap ((v,) . (.comp)) (find ((== entity) . (.target)) r))
      & qfoldr (\(From _ (Velocity x, Likes l)) y -> x * l + y) 0 e
      & qinsert (\x -> Pos $ x + p)

  assertqWith
    [q|Pos|]
    [(a, Pos 8), (b, Pos 29), (c, Pos 11), (d, Pos 16)]

test4 :: System ()
test4 = do
  a <- spawn (Name "A", Comp1)
  b <- spawn (Name "B", Rel ChildOf a, Comp1)
  c <- spawn (Name "C", Rel ChildOf a, Comp1)
  d <- spawn (Name "D", Comp1)
  e <- spawn (Name "E", Rel ChildOf d, Comp1)

  query_ $ do
    (e, name) <- [q|Entity, Name / With Comp1|]
    [q|/ With ChildOf -> e|]
      & qinsert (const name)

  assertqWith
    [q|Name / With Comp1|]
    [(a, Name "A"), (b, Name "A"), (c, Name "A"), (d, Name "D"), (e, Name "D")]

test5 :: System ()
test5 = do
  a <- spawn (Name "A", Comp1)
  b <- spawn (Name "B", Rel ChildOf a, Comp1)
  c <- spawn (Name "C", Rel ChildOf a, Comp1)
  d <- spawn (Name "D", Comp1)
  e <- spawn (Name "E", Rel ChildOf d, Comp1)

  [q|ChildOf -> (Name) / With Comp1|]
    & qinsert (\(From _ n) -> n)
    & query_

  assertqWith
    [q|Name / With Comp1|]
    [(a, Name "A"), (b, Name "A"), (c, Name "A"), (d, Name "D"), (e, Name "D")]
