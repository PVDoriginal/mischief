{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Data.Foldable
import GHC.Generics (Generic, type (:.:) (Comp1))
import Mischief.ECS

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Spawn"
        [ benchSpawnEntities 10,
          benchSpawnEntities 100,
          benchSpawnEntities 1000,
          benchSpawnEntities 10000,
          benchSpawnEntities 100000
        ]
    ]

benchSpawnEntities :: Int -> Benchmark
benchSpawnEntities a = bgroup (show a ++ " Entities") $ [benchSpawnComponents a b | b <- [2, 3, 5]]

benchSpawnComponents :: Int -> Int -> Benchmark
benchSpawnComponents a b = bgroup (show b ++ " Components") $ [env mkWorld (benchSpawnArchetypes a b c) | c <- [1, 2, 4, 5]]

benchSpawnArchetypes :: Int -> Int -> Int -> W -> Benchmark
benchSpawnArchetypes a b c w = bench (show c ++ " Archetypes") $ nfIO (benchSpawn a b c w)

mkWorld :: IO W
mkWorld = do
  app <- newApp P
  pure $ W app.world

newtype W = W World

instance NFData W where
  rnf _ = ()

-- The first Int indicates how many entities will be spawned. The second how many components each entity will have. The third how many archetypes the entities will be divided in.
benchSpawn :: Int -> Int -> Int -> W -> IO ()
benchSpawn ne nc na (W world) =
  runSystem (spawnEntities ne nc na) world

data P = P deriving (Eq, Plugin)

data Comp11 = Comp11 deriving (Component)

data Comp21 = Comp21 deriving (Component)

data Comp31 = Comp31 deriving (Component)

data Comp41 = Comp41 deriving (Component)

data Comp51 = Comp51 deriving (Component)

data Comp12 = Comp12 deriving (Component)

data Comp22 = Comp22 deriving (Component)

data Comp32 = Comp32 deriving (Component)

data Comp42 = Comp42 deriving (Component)

data Comp52 = Comp52 deriving (Component)

data Comp13 = Comp13 deriving (Component)

data Comp23 = Comp23 deriving (Component)

data Comp33 = Comp33 deriving (Component)

data Comp43 = Comp43 deriving (Component)

data Comp53 = Comp53 deriving (Component)

data Comp14 = Comp14 deriving (Component)

data Comp24 = Comp24 deriving (Component)

data Comp34 = Comp34 deriving (Component)

data Comp44 = Comp44 deriving (Component)

data Comp54 = Comp54 deriving (Component)

data Comp15 = Comp15 deriving (Component)

data Comp25 = Comp25 deriving (Component)

data Comp35 = Comp35 deriving (Component)

data Comp45 = Comp45 deriving (Component)

data Comp55 = Comp55 deriving (Component)

spawn' :: Int -> Int -> System Entity
spawn' 1 1 = spawn Comp11
spawn' 1 2 = spawn Comp12
spawn' 1 3 = spawn Comp13
spawn' 1 4 = spawn Comp14
spawn' 1 5 = spawn Comp15
spawn' 2 1 = spawn (Comp11, Comp21)
spawn' 2 2 = spawn (Comp12, Comp22)
spawn' 2 3 = spawn (Comp13, Comp23)
spawn' 2 4 = spawn (Comp14, Comp24)
spawn' 2 5 = spawn (Comp15, Comp25)
spawn' 3 1 = spawn (Comp11, Comp21, Comp31)
spawn' 3 2 = spawn (Comp12, Comp22, Comp32)
spawn' 3 3 = spawn (Comp13, Comp23, Comp33)
spawn' 3 4 = spawn (Comp14, Comp24, Comp34)
spawn' 3 5 = spawn (Comp15, Comp25, Comp35)
spawn' 4 1 = spawn (Comp11, Comp21, Comp31, Comp41)
spawn' 4 2 = spawn (Comp12, Comp22, Comp32, Comp42)
spawn' 4 3 = spawn (Comp13, Comp23, Comp33, Comp43)
spawn' 4 4 = spawn (Comp14, Comp24, Comp34, Comp44)
spawn' 4 5 = spawn (Comp15, Comp25, Comp35, Comp45)
spawn' 5 1 = spawn (Comp11, Comp21, Comp31, Comp41, Comp51)
spawn' 5 2 = spawn (Comp12, Comp22, Comp32, Comp42, Comp52)
spawn' 5 3 = spawn (Comp13, Comp23, Comp33, Comp43, Comp53)
spawn' 5 4 = spawn (Comp14, Comp24, Comp34, Comp44, Comp54)
spawn' 5 5 = spawn (Comp15, Comp25, Comp35, Comp45, Comp55)
spawn' _ _ = undefined

spawnEntities :: Int -> Int -> Int -> System ()
spawnEntities ne nc na =
  for_ [1 .. na] $ \a ->
    replicateM_ (ne `div` na) $
      spawn' nc a