module Mischief.ECS.EntityDef where

-- | Points to an unique entity. Has an id and a generation.
--
-- It is guaranteed that there can't be two alive entities with the same id.
data Entity = Entity {id :: Int, gen :: Int} deriving (Eq, Ord)

instance Show Entity where
  show :: Entity -> String
  show Entity {id, gen = 0} = show id ++ "v" ++ "."
  show entity = show entity.id ++ "v" ++ show entity.gen
