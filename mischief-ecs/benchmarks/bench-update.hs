module Main where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (replicateM_)
import Criterion.Main
import Criterion.Types
import Data.Foldable
import Mischief.ECS

main :: IO ()
main =
  defaultMainWith
    defaultConfig {reportFile = Just "/home/pvd/GameRepos/mischief/benches/update.html"}
    [ benchEntities 10,
      benchEntities 100,
      benchEntities 1000,
      benchEntities 10000,
      benchEntities 100000
    ]

benchEntities :: Int -> Benchmark
benchEntities a = bgroup (show a ++ " Entities") $ [env (mkWorld a) $ benchUpdates a b | b <- [1, 4, 8]]

benchUpdates :: Int -> Int -> W -> Benchmark
benchUpdates a b w = bench (show b ++ " Updates") $ nfIO $ benchUpdate a b w

benchUpdate :: Int -> Int -> W -> IO ()
benchUpdate _ b (W w) = replicateM_ b (runSystem up w)

data Comp1 = Comp1 deriving (Component)

newtype W = W World

instance NFData W where
  rnf _ = ()

mkWorld :: Int -> IO W
mkWorld n = do
  app <- newApp P
  runSystem (pre n) app.world
  pure $ W app.world

data P = P deriving (Eq, Plugin)

pre :: Int -> System ()
pre m = replicateM_ m (spawn Comp1)

up :: System ()
up = do
  q <- query (C @Comp1)
  for_ q $ flip set Comp1