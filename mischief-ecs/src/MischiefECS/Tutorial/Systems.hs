{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Systems Tutorial
-- Description: Tutorial on using @Systems@
--
-- This module contains a more in-depth tutorial on using @Mischief Systems@.
--
-- [Previous Chapter: Components]("MischiefECS.Tutorial.Components")
--
-- [Main Page]("MischiefECS")
module MischiefECS.Tutorial.Systems
  ( -- * Learn You an ECS for Great Mischief! - 3. Systems

    -- * Introduction
    -- $intro
  )
where

import Control.Monad.Reader
import MischiefECS

-- $intro
-- A 'System' in Mischief is simply a 'Monad' allowing operations to be executed on a 'World'.
-- Being a wrapper around @'ReaderT' 'World' 'IO'@, it also enabled 'IO' access through 'liftIO'.
--
-- Here is a simple system that spawns an entity and prints its @'Entity'@:
--
-- @
-- foo :: 'System' ()
-- foo = do
--   e <- 'spawn' ()
--   liftIO $ print e
-- @
--
-- Note that, unlike other ECS's, systems here are fully @composable@. Even the 'spawn' used above is a 'System' that we are calling from another 'System'!
--
-- In the functions provided by @Mischief@ you may see the 'MonadSystem' typeclass being used:
--
-- @
-- 'query' :: forall qd m w. ('Queryable' qd, 'MonadSystem' w m) => m ['QueryOutput' qd]
-- @
--
-- This makes the function accept other types of systems, besides just the normal 'System', such as 'ParSystem', but more on that later.
