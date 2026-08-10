module Mischief.ECS.World.Query.Markers where

newtype MR a b = MR b

data Has a = Has

newtype HasR a b = HasR b

data E = E

data C a = C

newtype R a b = R b

newtype R' a b = R' b

data Any = Any

data M a = M

newtype Q a = Q a

data Q' a b = Q' a b

newtype MQ a = MQ a

newtype Val a = Val a
