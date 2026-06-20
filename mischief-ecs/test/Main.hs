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

data A = A deriving (Queryable, Show)

instance Component A where
  required = require @B

data B = B deriving (Queryable, Show, Generic, Default)

instance Component B where
  required = require @C

data C = C deriving (Queryable, Generic, Default)

instance Component C where
  required = require @()

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
  e <- spawn (Name "Lol", B)
  _ <- spawn (Name "Lol2", A)

  remove @Name e
  q <- query' @Name $ with @B
  for_ q $ \name -> do
    liftIO $ print name

  Just x <- single' @(R Requires) $ with @(Meta A)
  liftIO $ print x.targets

  liftIO exitSuccess