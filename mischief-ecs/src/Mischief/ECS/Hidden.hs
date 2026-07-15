-- |
-- Internal module containing utilities for hiding data
-- behind constructors that can't be accessed externally.
--
-- Used for things such as hiding the 'Mischief.ECS.World.World' in a 'Mischief.ECS.World.ParSystem' so
-- it can't be mutated unsafely.
module Mischief.ECS.Hidden where

newtype Hidden a = Hidden a

hide :: a -> Hidden a
hide = Hidden

unhide :: Hidden a -> a
unhide (Hidden a) = a