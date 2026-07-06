-- |
-- Internal module containing utilities for hiding data
-- behind constructors that can't be accessed externally.
--
-- Used for things such as hiding the 'MischiefECS.World.World' in a 'MischiefECS.World.ParSystem' so
-- it can't be mutated unsafely.
module MischiefECS.Hidden where

newtype Hidden a = Hidden {get :: a}

class GetHidden b a where
  getHidden :: b -> a
