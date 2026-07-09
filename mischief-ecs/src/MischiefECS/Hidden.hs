-- |
-- Internal module containing utilities for hiding data
-- behind constructors that can't be accessed externally.
--
-- Used for things such as hiding the 'MischiefECS.World.World' in a 'MischiefECS.World.ParSystem' so
-- it can't be mutated unsafely.
module MischiefECS.Hidden where

newtype Hidden a = Hidden a

hide :: a -> Hidden a
hide = Hidden

unhide :: Hidden a -> a
unhide (Hidden a) = a