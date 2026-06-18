module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable hiding (and, or)
import Data.Maybe
import Data.Set qualified as Set
import GHC.Generics (Generic)
import MischiefECS
import Relationships (testRelationships)
import System.Exit (exitSuccess)
import Prelude hiding (and)

data A = A deriving (Queryable)

instance Component A where
  required = require @B

data B = B deriving (Queryable, Generic, Default)

instance Component B where
  required = require @C

data C = C deriving (Queryable, Generic, Default)

instance Component C where
  required = require @B

main :: IO ()
main = do
  -- testRelationships
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup setup

setup :: System ()
setup = do
  _ <- spawn (Name "Lol", C)
  let x = required @C
  liftIO $ print x

  iter' @Name (with @(B, C)) $ \name -> do
    liftIO $ print name