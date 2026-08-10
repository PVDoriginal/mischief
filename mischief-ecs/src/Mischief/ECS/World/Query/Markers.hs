{-# OPTIONS_GHC -Wno-unused-imports #-}

module Mischief.ECS.World.Query.Markers where

import Mischief.ECS.Components
import Mischief.ECS.Tables

-- | Used to query for the [Entity]('Mischief.ECS.Entities.Entity').
--
-- __Example__
--
-- @
-- x <- query E
-- @
--
-- __Quasi Notation__
--
-- * @Entity@
-- * @entity@
-- * @E@
-- * @e@
--
-- For instance:
--
-- @
-- x <- [q|Entity|]
-- @
data E = E

-- | Used to query for a component or to remove a component. Forces queries to only include entities which have the component.
--
-- __Example__
--
-- Querying:
--
-- @
-- x <- query (C \@Name)
-- @
--
-- Removing:
--
-- @
-- remove (C \@Name)
-- @
--
-- __Quasi Notation__
--
-- None, you just write the component name directly. For instance:
--
-- @
-- x <- [q|Name|]
-- @
data C a = C

-- | Used to query for a component that an entity may or may not have.
--
-- __Example__
--
-- @
-- x <- query (M \@Name)
-- @
--
-- __Quasi Notation__
--
-- * @Maybe@
-- * @maybe@
-- * @M@
-- * @m@
--
-- For instance:
--
-- @
-- x <- [q|Maybe Name|]
-- @
data M a = M

-- | Used in queries to return a Bool which indicates whether a component exists on the entity or not.
--
-- __Example__
--
-- @
-- x <- query (Has \@Name)
-- @
--
-- __Quasi Notation__
--
-- * @Has@
-- * @has@
-- * @H@
-- * @h@
--
-- For instance:
--
-- @
-- x <- [q|Has Name|]
-- @
data Has a = Has

-- | Wildcard used when querying for relationships to indicate that you're querying for relationships with any target.
--
-- In the case of exclusive relationships, @Any@ returns a single relationship rather than a list of relationships.
data Any = Any

-- | Used to query for a relationships. Can be used in three different ways:
--
-- __1. @R c e@__
--
-- Queries for a relationship with a specific target. Forces queries to only include entities which have such a relationship.
--
-- __Example__
--
-- @
-- x <- query (R \@Likes alice)
-- @
--
-- @
-- x :: [Result (Rel Likes)]
-- @
--
-- __Quasi Notation__
--
-- It is written with an arrow like so:
--
-- @
-- x \<- [q|Likes -\> alice|]
-- @
--
-- __2. @R c Any@__
--
-- Queries for all relationships with any target. Forces queries to only include entities which have at least one such relationship.
--
-- __Example__
--
-- @
-- x <- query (R \@Likes Any)
-- @
--
-- @
-- x :: [[Result (Rel Likes)]]
-- @
--
-- __Quasi Notation__
--
-- @
-- x \<- [q|Likes -\> *|]
-- @
--
-- __3. @R c q@__
--
-- Runs a transitive query on the targets of all such relationships.
-- q can either be @Q d@ or @Q' d f@, depending if you want to also run a filter or not.
--
-- Similar to the previous case, it will limit the query to only entities which have such a relationship with at least one entity which matches the transitive query.
--
-- __Example__
--
-- @
-- x <- query (R @Likes (Q (C \@Name))
-- @
--
-- @
-- x <- query (R @Likes (Q' (C \@Name) (With (C \@Enemy))))
-- @
--
-- @
-- x :: [[Result Name]]
-- @
--
-- __Quasi Notation__
--
-- You just write the given query in the @()@ following the arrow. @\/@ can be used to give it a filter.
--
-- For instance:
--
-- @
-- x \<- [q|Likes -\> (Name)|]
-- @
--
-- @
-- x \<- [q|Likes -\> (Name / With Enemy)|]
-- @
newtype R a b = R b

-- | Exactly like 'R', except that it wraps the result in a Maybe, also including entities that don't have such a relationship.
--
-- __Example__
--
-- @
-- x <- query (MR @Likes Any)
-- @
--
-- __Quasi Notation__
--
-- * @Maybe@
-- * @maybe@
-- * @M@
-- * @m@
--
-- For instance:
--
-- @
-- x <- [q|Maybe Likes -> *|]
-- @
newtype MR a b = MR b

-- | Exactly like 'R', except that it returns a Bool depending on whether the entity has such a relationship or not.
--
-- __Example__
--
-- @
-- x <- query (HasR @Likes Any)
-- @
--
-- __Quasi Notation__
--
-- * @Has@
-- * @has@
-- * @H@
-- * @h@
--
-- For instance:
--
-- @
-- x <- [q|Has Likes -> *|]
-- @
newtype HasR a b = HasR b

-- | Used when writing transitive queries. See 'R'.
newtype Q a = Q a

-- | Used when writing transitive queries with filters. See 'R'.
data Q' a b = Q' a b

-- | Can be wrapped around any queryable marker to unwrap the inner value from the Results.
--
-- __Example__
--
-- @
-- x <- query (C \@Name, Val (C \@Name))
-- @
--
-- @
-- x :: [(Result Name, Name)]
-- @
--
-- @
-- y <- query (Val (C \@Name, R \@Likes Any))
-- @
--
-- @
-- y :: [(Name, [Rel Likes])]
-- @
--
-- __Quasi Notation__
--
-- * @Val@
-- * @val@
-- * @V@
-- * @v@
-- * @*@
--
-- For instance:
--
-- @
-- x <- [q|Name, *Name|]
-- @
--
-- @
-- x <- [q|*(Name, Likes -> *)|]
-- @
newtype Val a = Val a

newtype R' a b = R' b