module Mischief.ECS.World.Query.Validation where

class ValidateQuery qd where
  validate :: qd -> Bool
