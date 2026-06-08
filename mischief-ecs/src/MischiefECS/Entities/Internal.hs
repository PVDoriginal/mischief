module MischiefECS.Entities.Internal where

data Entity = Entity {id :: Int, gen :: Int} deriving (Show, Eq, Ord)
