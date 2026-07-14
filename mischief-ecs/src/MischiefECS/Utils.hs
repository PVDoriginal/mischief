module MischiefECS.Utils where

import Data.Data
import Data.Foldable
import Data.Kind
import Data.Maybe
import Data.Text (Text)

class GetRep t where
  getRep :: t -> TypeRep

findM :: forall (m :: Type -> Type) a. (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM f (x : xs) = do
  b <- f x
  if b
    then
      pure $ Just x
    else
      findM f xs

unwrap :: Maybe a -> a
unwrap = fromMaybe undefined
