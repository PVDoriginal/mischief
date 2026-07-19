{- HLINT ignore "Use newtype instead of data" -}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
-- import Relationships (testRelationships)

import Data.Data
import Data.Default
import Data.Foldable hiding (and, or)
import Data.Maybe
import Data.Set qualified as Set
import GHC.Generics (Generic)
import GHC.StableName
import Mischief.ECS
import Mischief.ECS.App.Plugins
import Mischief.ECS.App.Systems
import Mischief.ECS.Collectable
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.Hooks
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Hooks
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Utils
import System.Exit (exitSuccess)

data Player = Player deriving (Component)

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    insertRes $ PlayerCount 0
    Systems.add Update (updateCount, spawnPlayers)
    void . spawn $ Observer handlePlayerRemove

spawnPlayers :: System ()
spawnPlayers = do
  Just x <- res @PlayerCount
  when (x.inner < 10) $ do
    (info . text) =<< res @PlayerCount

    p <- spawn Player
    void $ spawn Player
    void $ spawn Player

    despawn p

data PlayerCount = PlayerCount {inner :: Int} deriving (Component, Show)

changeCount :: Int -> PlayerCount -> PlayerCount
changeCount n (PlayerCount x) = PlayerCount (x + n)

updateCount :: System ()
updateCount = do
  x <- query' E (Added @Player)
  Just count <- res @PlayerCount
  modify count $ changeCount (length x)
  err $ text count

handlePlayerRemove :: OnRemove Player -> System ()
handlePlayerRemove _ = do
  Just count <- res @PlayerCount
  modify count $ changeCount (-1)