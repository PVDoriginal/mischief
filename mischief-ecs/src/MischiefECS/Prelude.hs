module MischiefECS.Prelude
  ( Entity,
    despawn,
    Component,
    required,
    require,
    Queryable,
    query,
    remove,
    System,
    Plugin,
    newApp,
    runApp,
    addSystems,
    addPlugin,
    Name (Name),
    ComponentResult (value),
  )
where

import MischiefECS.App (Plugin, addPlugin, addSystems, newApp, runApp)
import MischiefECS.Components (Component (required))
import MischiefECS.Components.Default (require)
import MischiefECS.Entities (Entity)
import MischiefECS.Tables (ComponentResult (value))
import MischiefECS.World (Name (Name), System, despawn)
import MischiefECS.World.Query (Queryable, query)
import MischiefECS.World.Remove (remove)
