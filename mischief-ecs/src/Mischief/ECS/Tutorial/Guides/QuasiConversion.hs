{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: Query Basics]("Mischief.ECS.Tutorial.Guides.QueryBasics")
--
-- [Next Chapter: Query Piping]("Mischief.ECS.Tutorial.Guides.QueryPiping")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.QuasiConversion
  ( -- * Learn You an ECS for Great Mischief! - 2.4. Quasi-Query Conversion
    -- $intro

    -- * Query Markers
    -- $markers

    -- * Converting @q@
    -- $q

    -- * [Next Chapter: Query Piping]("Mischief.ECS.Tutorial.Guides.QueryPiping")
  )
where

import Mischief.ECS

-- $intro
-- This chapter will teach you to write queries without quasi-notation (such as @[q|..|]@).
-- It is extremely optional and you may never want or need to use the normal notation,
-- so feel free to skip past it.

-- $markers
-- To write a query without the quasi-quoter, you ned to use markers. They are
-- simple abstract types that define the sort of thing you're querying for.
--
-- You can query for a component using @'C'@.
--
-- @
-- ['qd'|Name, Health|]
-- @
--
-- @
-- ('C' \@Name, 'C' \@Health)
-- @
--
-- You can query for a relationship using @'R'@.
--
-- @
-- ['qd'|Likes -> *, ChildOf -> bob|]
-- @
--
-- @
-- ('R' \@Likes 'Any', 'R' \@ChildOf bob)
-- @
--
-- You can run transitive queries using @'Q'@ (or @'Q''@ to also provide a filter):
--
-- @
-- ['qd'|Likes -> (Name), ChildOf -> (Health / With Player)|]
-- @
--
-- @
-- ('R' \@Likes ('Q' ('C' \@Name)), 'R' \@ChildOf ('Q'' ('C' \@Health) ('With' ('C' \@Player))))
-- @
--
-- You can query for optional components using @M@. And for optional relationships using @MR@.
--
-- @
-- ['qd'|Maybe Name, Maybe Likes -> *|]
-- @
--
-- @
-- ('M' \@Name, 'MR' \@Likes 'Any')
-- @
--
-- Each filter takes a single @C@ or @R@ type. They must be chained with @And@, @Or@, @Not@.
--
-- @
-- ['qf'|With Player, Without Likes -> * || !Changed Health|]
-- @
--
-- @
-- 'With' ('C' \@Player) '`And`' ('Without' ('R' \@Likes 'Any') '`Or`' 'Not' ('Changed' ('C' \@Health)))
-- @

-- $q
-- The @q@ quasi-quoter will generate one of four functions, based on the following context:
--
-- If you query for components with no filter:
--
-- @
-- ['q'|Name, Health|]
-- @
--
-- @
-- 'mkQuery' ('C' \@Name, 'C' \@Health)
-- @
--
-- If you query for components with a filter:
--
-- @
-- ['q'|Name / With Player|]
-- @
--
-- @
-- 'mkQuery'' ('C' \@Name) ('With' ('C' \@Player))
-- @
--
-- If you query for components of a specific entity with no filter:
--
-- @
-- ['q'|bob. Name, Health|]
-- @
--
-- @
-- 'mkGet' bob ('C' \@Name, 'C' \@Health)
-- @
--
-- If you query for components of a specific entity with a filter:
--
-- @
-- ['q'|bob. Name / With Player|]
-- @
--
-- @
-- 'mkGet'' bob ('C' \@Name) ('With' ('C' \@Player))
-- @