{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: Entities and Components Basics]("Mischief.ECS.Tutorial.Guides.Basics")
--
-- [Next Chapter: Quasi-Query Conversion]("Mischief.ECS.Tutorial.Guides.QuasiConversion")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.QueryBasics
  ( -- * Learn You an ECS for Great Mischief! - 2.3. Query Basics
    -- $intro

    -- * Query Data
    -- $qd

    -- * Query Filters
    -- $qf

    -- * What about @q@?
    -- $q

    -- * What was that about a Query?
    -- $query

    -- * [Next Chapter: Quasi-Query Conversion]("Mischief.ECS.Tutorial.Guides.QuasiConversion")
  )
where

import Mischief.ECS

-- $intro
-- In an ECS, queries are a mechanism used to read data based on a certain user-defined format. In Mischief, they're a bit more complex than that;
-- queries are pure functions chained together which can trigger effects in the World, as well as retrieve data.
--
-- This chapter is an introduction to the @q@ quasi-quoter, and a short how-to guide on reading data from entities. The next chapters
-- delve into more complex queries.

-- $qd
-- Query Data is a tuple of types that are queryable. Query Data can be written using the @qd@ macro. Here are the things you can query for:
--
-- * Components: get a component's value.
--
-- @
-- ['qd'|Name, Health|]
-- @
--
-- * Relationships: get relationships with a specific component. Either all relationships, or just with a specific entity. This will return either a
-- single @Rel@ or a list of them.
--
-- @
-- ['qd'|Likes -> *|]
-- @
--
-- @
-- ['qd'|Likes -> bob|]
-- @
--
-- * Transitive Components: similar to the previous but you give it an entire query to grab from each of the targets. Will return a @From@ or a list of them.
--
-- @
-- ['qd'|Likes -> (Name, Health)|]
-- @
--
-- * Resource: Grabs a resource from the world.
--
-- @
-- ['qd'|Res SomeRes]
-- @
--
-- When you query for each of these types, the query will filter out any entities that don't have them.
-- For instance, if you're querying for @Health@, you're leaving out entities that don't have @Health@, and if
-- you're querying for @Likes -> *@ you're leaving out entities that like no one.
--
-- In order to include optional components, resources, relationships, you can use the @Maybe@ keyword:
--
-- @
-- ['qd'|Name, Maybe Health, Maybe Likes -> (Name)]
-- @

-- $qf
-- Query Filters can be used to filter the entities that a query is looking at. They can be written with the @qf@ quasi-quoter.
--
-- There are four such base filters:
--
-- * With: filters out entities that do not have this component.
--
-- @
-- ['qf'|With Player|]
-- @
--
-- * Without: filters out entities that have this component.
--
-- @
-- ['qf'|Without Enemy|]
-- @
--
-- * Changed: filters out entities that haven't had this component be re-inserted since the current system last ran:
--
-- @
-- ['qf'|Changed Name|]
-- @
--
-- * Added: filters out entities that haven't had this component added since the current system last ran:
--
-- @
-- ['qf'|Added Health|]
-- @
--
-- There are two types of filters: Archetype Filters, and Entity Filters. Only @With@ and @Without@ can be Archetype Filters,
-- but all four can be Entity Filters.
--
-- Filters can be combined with @&&@, @||@ and @!@(Not). Separating them by @,@ will result in an implicit @&&@:
--
-- @
-- ['qf'|Added Health, Changed Name|]
-- @
--
-- Is the same as:
--
-- @
-- ['qf'| Added Health && Changed Name|]
-- @

-- $q
-- @q@ combines the previous two quasi-quoters, and produces a @Query@.
--
-- For instance, the following will result in a @Query (Name, Player)@:
--
-- @
-- ['q'|Name, Player / Without Enemy|]
-- @
--
-- The first part (left of the @/@) is the same as @qd@, while the other is the same as @qf@. The @/@ along with the filter can be omitted. Note that
-- the filter in @q@ must be an Archetype Filter!
--
-- Additionally, @q@ accepts an Entity at the start, filling the Query with just the components from that Entity:
--
-- @
-- ['q'|bob. Name, Health|]
-- @
--
-- Grabbing the @Name@ and @Health@ of /just/ bob is an @O(1)@ operation.

-- $query
-- @Query (Name, Player)@ is a Query through which the @Name@ and @Player@ components flow. You can think of it
-- as a wrapper around @[(Name, Player)]@, although it's a bit more complicated than that.
--
-- The list of components can be extracted from a Query using @query@ or @single@:
--
-- @
-- names \<- 'query' ['q'|Name|]
-- @
--
-- @
-- 'Just' bobName \<- 'single' ['q'|bob. Name|]
-- @
--
-- @single@ is the same as @query@ but instead of returning a list, it returns a @Maybe@ depending on if that list had exactly one element or not. It is useful
-- if you're retrieving the components of a specific entity or if you know there's only one entity that could fit your query (for instance, there is only one Player spawned).
