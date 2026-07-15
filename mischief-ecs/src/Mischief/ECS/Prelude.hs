module Mischief.ECS.Prelude
  ( Entity,
    Component,
    required,
    query,
    remove,
  )
where

-- import Mischief.ECS.App (Plugin, addSystems, newApp, runApp)
import Mischief.ECS.Components (Component (required))
import Mischief.ECS.Entities (Entity)
import Mischief.ECS.World.Query (query)
import Mischief.ECS.World.Remove (remove)
