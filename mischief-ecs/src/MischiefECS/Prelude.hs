module MischiefECS.Prelude
  ( Entity,
    Component,
    required,
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
  )
where

import MischiefECS.App (Plugin, addPlugin, addSystems, newApp, runApp)
import MischiefECS.Components (Component (required))
import MischiefECS.Entities (Entity)
import MischiefECS.World (Name (Name), System)
import MischiefECS.World.Query (Queryable, query)
import MischiefECS.World.Remove (remove)
