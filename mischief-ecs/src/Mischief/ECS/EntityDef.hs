module Mischief.ECS.EntityDef where

import GHC.Base (Word (W#), Word#, compareWord#, eqWord#, isTrue#)

-- |
-- __UNLIFTED__
--
-- Points to an unique entity. The first @Word#@ is an id, the second is a generation.
--
-- It is guaranteed that there can't be two alive entities with the same id.
data Entity = Entity (# Word#, Word# #)

instance Show Entity where
  show :: Entity -> String
  show (Entity (# id, 0## #)) = show (W# id) ++ "v" ++ "."
  show (Entity (# id, gen #)) = show (W# id) ++ "v" ++ show (W# gen)

instance Eq Entity where
  (==) :: Entity -> Entity -> Bool
  (==) (Entity (# id1, gen1 #)) (Entity (# id2, gen2 #)) = isTrue# (eqWord# id1 id2) && isTrue# (eqWord# gen1 gen2)

instance Ord Entity where
  compare :: Entity -> Entity -> Ordering
  compare (Entity (# id1, gen1 #)) (Entity (# id2, gen2 #)) =
    case compareWord# id1 id2 of
      EQ -> compareWord# gen1 gen2
      x -> x