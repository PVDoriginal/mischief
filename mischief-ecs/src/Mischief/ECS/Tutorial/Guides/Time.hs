{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Queries Tutorial
-- Description: How To Mischief.
--
-- [Previous Chapter: Scheduling and Running Systems]("Mischief.ECS.Tutorial.Guides.Scheduling")
--
-- [Next Chapter: Parallelism and Asynchronicity]("Mischief.ECS.Tutorial.Guides.Parallelism")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Time
  ( -- * Learn You an ECS for Great Mischief! - 2.8. Keeping Track of Time
    -- $intro
  )
where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Traversable (for)
import Mischief.ECS

-- $intro
-- TODO