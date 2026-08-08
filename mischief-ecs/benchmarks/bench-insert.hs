module Main where

import Control.DeepSeq (NFData (rnf))
import Control.Monad
import Criterion
import Criterion.Main
import Criterion.Types
import Data.Foldable
import Data.Traversable
import Mischief.ECS

main :: IO ()
main =
  defaultMainWith
    defaultConfig {reportFile = Just "/home/pvd/GameRepos/mischief/benches/insert.html"}
    [ benchInsert 10,
      benchInsert 100,
      benchInsert 1000,
      benchInsert 10000,
      benchInsert 100000
    ]

benchInsert :: Int -> Benchmark
benchInsert n = bgroup (show n ++ " Entities") [env (mkEs n) benchF]

benchF :: (Es, W) -> Benchmark
benchF a = bench "" $ nfIO $ benchI a

benchI :: (Es, W) -> IO ()
benchI (Es es, W w) = runSystem (insertComponents es) w

mkEs :: Int -> IO (Es, W)
mkEs n = do
  app <- newApp P
  es <- runSystem (replicateM n (spawn ())) app.world
  pure (Es es, W app.world)

data P = P deriving (Eq, Plugin)

data Comp1 = Comp1 deriving (Component)

newtype Es = Es [Entity]

newtype W = W World

instance NFData Es where
  rnf _ = ()

instance NFData W where
  rnf _ = ()

insertComponents :: [Entity] -> System ()
insertComponents = traverse_ (insert Comp1)
