module Mischief.ECS.Utils where

import Data.Data
import Data.Foldable
import Data.Kind
import Data.Maybe
import GHC.Stack

class GetRep t where
  getRep :: t -> TypeRep

findM :: forall (m :: Type -> Type) a t. (Monad m, Foldable t) => (a -> m Bool) -> t a -> m (Maybe a)
findM f l = f' f (toList l)
  where
    f' _ [] = pure Nothing
    f' f (x : xs) = do
      b <- f x
      if b
        then
          pure $ Just x
        else
          findM f xs

unwrap :: (HasCallStack) => Maybe a -> a
unwrap = withFrozenCallStack $ fromMaybe undefined

(>>+) :: forall (m :: Type -> Type) a b. (Monad m) => m a -> (a -> m b) -> m a
(>>+) a f = a >>= f >>= const a

expect :: (HasCallStack) => String -> Maybe a -> a
expect t a = withFrozenCallStack $
  case a of
    Nothing -> error t
    Just x -> x