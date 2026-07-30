{- HLINT ignore "Use newtype instead of data" -}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Mischief.ECS
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Systems qualified as Systems

data Likes = Likes Int deriving (Show)

instance Component Likes where
  hooks = Hooks.relCleanupRemove

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = Systems.add Startup test

test :: System ()
test = do
  alice <- spawn (Name "Alice")
  bob <- spawn (Name "Bob")
  charlie <- spawn (Name "Charlie")

  insert (Rel (Likes 5) alice, Rel (Likes 10) bob) charlie
  insert (Rel (Likes 7) alice) bob
  insert (Rel (Likes 9) bob, Rel (Likes 5) charlie) alice

  (info . text) =<< query (C @Name, R @Likes alice)
  (info . text) =<< query (C @Name, R @Likes bob)
  (info . text) =<< query (C @Name, R @Likes charlie)

  remove (R @Likes Any) alice
  despawn bob

  (info . text) =<< query (C @Name, R @Likes alice)
  (info . text) =<< query (C @Name, R @Likes bob)
  (info . text) =<< query (C @Name, R @Likes charlie)
