module MischiefECS.Utils where

import Data.Data
import Data.Foldable
import Data.Kind
import Data.Maybe

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

unwrap :: Maybe a -> a
unwrap = fromMaybe undefined

(>>+) :: forall (m :: Type -> Type) a b. (Monad m) => m a -> (a -> m b) -> m a
(>>+) a f = a >>= f >>= const a