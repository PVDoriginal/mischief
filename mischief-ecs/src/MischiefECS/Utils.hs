module MischiefECS.Utils where

import Data.Data

class GetRep t where
  getRep :: t -> TypeRep

newtype Tick = Tick Int deriving (Show, Eq, Ord)
