module Mischief.ECS.Entities where

data Entity = Entity {id :: Int, gen :: Int}

instance Eq Entity

instance Ord Entity

instance Show Entity