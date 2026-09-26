{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Mischief.ECS.Exports
  ( module Data.String.Interpolate,
    module Data.Default,
    module GHC.Generics,
  )
where

import Data.Default (Default (..))
import Data.String.Interpolate (i)
import GHC.Generics (Generic)
