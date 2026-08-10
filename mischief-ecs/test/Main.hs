{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}

{- HLINT ignore "Use newtype instead of data" -}

module Main where

import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.List ((!?))
import Data.Traversable
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Interval qualified as Interval
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude
import Mischief.ECS.Stdin qualified as Stdin
import Mischief.ECS.Stdout
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.Timer (Timer)
import Mischief.ECS.Timer qualified as Timer
import System.Exit
import System.Random
import System.Random.Stateful

data Likes = Likes Int deriving (Show)

instance Component Likes where
  hooks = Hooks.relCleanupRemove

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    Stdin.init
    Systems.add Startup (spawnGrid, spawnWalls)
    Systems.add Update printGrid

    interval <- Interval.start 2000000 spawnCoin

    insertRes =<< newGen
    insertRes $ Coins 0

  plugins _ = plug (PlayerPlugin, EnemyPlugin, TimePlugin)

data PlayerPlugin = PlayerPlugin deriving (Eq)

instance Plugin PlayerPlugin where
  init _ = do
    Systems.add Startup $ spawnPlayer `after` spawnGrid
    Systems.add Update movePlayer
    Systems.add Update $ collectCoins `after` movePlayer

    void $ Observers.spawn onDamage

data EnemyPlugin = EnemyPlugin deriving (Eq)

instance Plugin EnemyPlugin where
  init _ = do
    Systems.add Startup spawnEnemies
    Systems.add Update moveEnemies
    Systems.add Update $ tryDamage `after` movePlayer `after` moveEnemies

data Tile = Tile deriving (Component)

newtype Pos = Pos {pos :: (Int, Int)} deriving (Component, Show)

data Grid = Grid [[Entity]] deriving (Component)

getTile :: (Int, Int) -> System (Maybe Entity)
getTile (x, y) = do
  grid <- res @Grid
  pure $ do
    Grid tiles <- grid
    line <- tiles !? x
    line !? y

gridH :: Int
gridH = 10

gridW :: Int
gridW = 20

spawnGrid :: System ()
spawnGrid = do
  tiles <- for [0 .. gridH - 1] $ \i -> for [0 .. gridW - 1] $ \j ->
    spawn (Tile, Pos (i, j))

  insertRes $ Grid tiles

moveBy :: (Int, Int) -> Entity -> System (Maybe Entity)
moveBy (x, y) entity = do
  Just (Pos (x', y')) <- [g|*Pos|] entity
  getTile (x' + x, y' + y)

data Player = Player

instance Component Player where
  required = require @Health

data OnTile = OnTile

instance Component OnTile where
  type RelExclusivity OnTile = Exclusive

spawnPlayer :: System ()
spawnPlayer = do
  Just tile <- getTile (5, 5)
  void $ spawn (Player, Rel OnTile tile)

data Wall = Wall deriving (Component)

spawnWall :: (Int, Int) -> System Entity
spawnWall pos = do
  Just tile <- getTile pos
  spawn (Wall, Rel OnTile tile)

spawnWalls :: System ()
spawnWalls = do
  for_ [0 .. gridW - 1] $ \i -> spawnWall (0, i)
  for_ [0 .. gridW - 1] $ \i -> spawnWall (gridH - 1, i)
  for_ [1 .. gridH - 2] $ \i -> spawnWall (i, 0)
  for_ [1 .. gridH - 2] $ \i -> spawnWall (i, gridW - 1)

showTile :: Entity -> System Char
showTile tile = do
  player <- tileHas @Player tile
  enemy <- tileHas @Enemy tile
  wall <- tileHas @Wall tile
  coin <- tileHas @Coin tile

  pure $
    if
      | player -> '@'
      | wall -> '#'
      | enemy -> '!'
      | coin -> '$'
      | otherwise -> '.'

showGrid :: System String
showGrid = do
  Just (Grid tiles) <- res @Grid
  lines <- for tiles $ traverse showTile
  return $ unlines lines

showHealth :: System String
showHealth = do
  Just health <- [s|Health / With Player|]
  pure $ "Health: " ++ show health.hp

showCoins :: System String
showCoins = do
  Just (Coins c) <- res @Coins
  pure $ "Coins: " ++ show c

printGrid :: System ()
printGrid = do
  grid <- showGrid
  health <- showHealth
  coins <- showCoins
  printClear $ health ++ "\n" ++ grid ++ "\n" ++ coins ++ "\n"

movePlayer :: System ()
movePlayer = do
  c <- Stdin.readLast
  for_ c $ \case
    'w' -> movePlayerBy (-1, 0)
    's' -> movePlayerBy (1, 0)
    'a' -> movePlayerBy (0, -1)
    'd' -> movePlayerBy (0, 1)
    _ -> pure ()

movePlayerBy :: (Int, Int) -> System ()
movePlayerBy dir = do
  Just player <- single' E (With (C @Player))

  Just (tile, pos) <- [g|OnTile -> (Entity, *Pos)|] player
  newTile <- moveBy dir tile

  for_ newTile $ \t ->
    tileIsFree t >>= flip when (insert (Rel OnTile t) player)

hasWall :: Entity -> System Bool
hasWall = tileHas @Wall

data Enemy = Enemy

instance Component Enemy where
  required = require @Cooldown

data Cooldown = Cooldown {timer :: Timer} deriving (Component)

instance Default Cooldown where
  def = Cooldown $ Timer.new 0.5 Timer.Repeat

data Rand = Rand (IOGenM StdGen) deriving (Component)

newGen :: System Rand
newGen = Rand <$> (newIOGenM =<< initStdGen)

randomPos :: System (Int, Int)
randomPos = do
  Just (Rand gen) <- res @Rand
  i <- applyIOGen (uniformR (1, gridH - 1)) gen
  j <- applyIOGen (uniformR (1, gridW - 1)) gen
  return (i, j)

randomTile :: System Entity
randomTile = unwrap <$> (getTile =<< randomPos)

spawnEnemy :: System Entity
spawnEnemy = do
  tile <- randomTile
  spawn (Enemy, Rel OnTile tile)

spawnEnemies :: System ()
spawnEnemies = for_ [0 .. 4] $ const spawnEnemy

decideEnemyDir :: Pos -> Pos -> System (Int, Int)
decideEnemyDir (Pos (ex, ey)) (Pos (px, py)) = do
  left <- tileAtPosIsFree (ex - 1, ey)
  up <- tileAtPosIsFree (ex, ey - 1)
  right <- tileAtPosIsFree (ex + 1, ey)
  down <- tileAtPosIsFree (ex, ey + 1)

  pure $
    if
      | ex > px && left -> (-1, 0)
      | ey > py && up -> (0, -1)
      | ex < px && right -> (1, 0)
      | ey < py && down -> (0, 1)
      | otherwise -> (0, 0)

moveEnemies :: System ()
moveEnemies = do
  Just pos <- [s|OnTile -> (*Pos) / With Player|]
  delta <- deltaTime

  enemies <- [q|Entity, OnTile -> (Entity, *Pos), Cooldown / With Enemy|]
  for_ enemies $ \(enemy, (enemyTile, enemyPos), cooldown) -> do
    let (timer, finished) = Timer.tick delta cooldown.timer
    set cooldown $ Cooldown timer

    when finished $ do
      diff <- decideEnemyDir enemyPos pos

      newTile <- moveBy diff enemyTile
      for_ newTile $ \t -> do
        insert (Rel OnTile t) enemy

tileHas :: forall c. (QueryType c) => Entity -> System Bool
tileHas tile = not . null <$> [q|Entity / With (c, OnTile -> tile)|]

tileAtPosIsFree :: (Int, Int) -> System Bool
tileAtPosIsFree pos = do
  tile <- getTile pos
  maybe (pure False) tileIsFree tile

tileIsFree :: Entity -> System Bool
tileIsFree tile = do
  wall <- tileHas @Wall tile
  enemy <- tileHas @Enemy tile
  player <- tileHas @Player tile
  pure $ not (wall || enemy || player)

data Health = Health {hp :: Int} deriving (Component)

instance Default Health where
  def = Health 100

data Damage = Damage {amount :: Int} deriving (Event)

onDamage :: Damage -> System ()
onDamage dmg = do
  player <- [s|(Entity, Health) / With Player, Without Invincible|]

  for_ player $ \(entity, health) -> do
    modify health $ \(Health x) -> Health $ max (x - dmg.amount) 0

    insert Invincible entity
    delay 1000000 $ remove (C @Invincible) entity

    Just health <- update health
    when (health.hp == 0) $ liftIO exitSuccess

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (Pos (x1, y1)) (Pos (x2, y2)) =
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
   in (dx == 1 && dy == 0) || (dx == 0 && dy == 1)

tryDamage :: System ()
tryDamage = do
  Just player <- [s|OnTile -> (*Pos) / With Player|]
  enemies <- [q|OnTile -> (*Pos) / With Enemy|]

  for_ enemies $ \pos -> do
    when (isAdjacent pos player) $ do
      trigger (Damage 5)

data Invincible = Invincible deriving (Component)

data Coin = Coin deriving (Component)

spawnCoin :: System ()
spawnCoin = do
  tile <- randomTile
  free <- tileIsFree tile
  if free
    then
      void $ spawn (Coin, Rel OnTile tile)
    else
      spawnCoin

data Coins = Coins Int deriving (Component)

collectCoins :: System ()
collectCoins = do
  Just playerTile <- [s|OnTile -> (Entity) / With Player|]
  coins <- [q|Entity / With OnTile -> playerTile, With Coin|]

  Just (Coins c) <- res @Coins
  insertRes $ Coins $ c + length coins

  for_ coins despawn
