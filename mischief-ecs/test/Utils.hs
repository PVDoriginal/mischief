module Utils where

import Data.List (sortBy, sortOn)
import Mischief.ECS (App (..), Component, MonadSystem, System, runSystem)
import Mischief.ECS.EntityDef
import Mischief.ECS.Prelude

runTests :: [System ()] -> IO ()
runTests [] = pure ()
runTests (x : xs) = do
  app <- newApp
  sys app x
  runTests xs

sys :: App -> System () -> IO ()
sys app s = runSystem s app.world

assertEq :: (Eq a, Show a, Applicative m) => a -> a -> m ()
assertEq a b | a == b = pure ()
assertEq a b = error $ "assert eq failed:\n" ++ show a ++ "\n" ++ show b

assertq :: (Eq a, Show a, MonadSystem w m) => Query m a -> Query m a -> m ()
assertq a b = do
  a <- sortOn fst <$> query (qentity a)
  b <- sortOn fst <$> query (qentity b)
  assertEq a b

assertqWith :: (Eq a, Show a, MonadSystem w m) => Query m a -> [(Entity, a)] -> m ()
assertqWith a b = do
  a <- sortOn fst <$> query (qentity a)
  assertEq a (sortOn fst b)

newtype Pos = Pos Int deriving (Component, Eq, Show)

newtype Velocity = Velocity Int deriving (Component, Eq, Show)

newtype Likes = Likes Int deriving (Component, Eq, Show)

data Comp1 = Comp1 deriving (Component, Eq, Show)

data Comp2 = Comp2 deriving (Component, Eq, Show)

data Comp3 = Comp3 deriving (Component, Eq, Show)