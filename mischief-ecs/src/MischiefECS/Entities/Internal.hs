module MischiefECS.Entities.Internal where

data Entity = Entity {id :: Int, gen :: Int} deriving (Eq, Ord)

instance Show Entity where
  show :: Entity -> String
  show Entity {id, gen = 0} = show id ++ "v" ++ "."
  show entity = show entity.id ++ "v" ++ show entity.gen
