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
import MischiefECS.Components.Spawn
import Relationships (testRelationships)
import System.Exit (exitSuccess)
import Prelude hiding (and)

data A = A deriving (Queryable, Show)

instance Component A where
  required = require @B

data B = B deriving (Queryable, Show, Generic, Default)

instance Component B where
  required = require @C

data C = C deriving (Queryable, Generic, Default, Show)

instance Component C where
  required = require @()

main :: IO ()
main = do
  -- testRelationships
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  register @(A, B)
  register @ObserverOrder
  register @ComponentArchetypes

  initRes @C

  addSystems Startup setup
  addSystems Update dummy

setup :: System ()
setup = do
  e <- spawn (A, Name "Lol")
  Just (name, _) <- get @(Name, (A, B)) e

  liftIO $ print name

dummy :: System ()
dummy = return ()