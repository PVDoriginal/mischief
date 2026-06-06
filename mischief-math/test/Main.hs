module Main where

import Data.Default
import MischiefMath
import MischiefMath.Mat qualified as Mat
import MischiefMath.Quat qualified as Quat

quat = Quat.rotateX 1.7 Quat.identity

main :: IO ()
main = print $ Quat.toEuler quat