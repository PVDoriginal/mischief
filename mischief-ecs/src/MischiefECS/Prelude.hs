module MischiefECS.Prelude
  ( Entity,
    Component,
    required,
    query,
    remove,
    Plugin,
    newApp,
    runApp,
    addSystems,
    addPlugin,
  )
where

import MischiefECS.App (Plugin, addPlugin, addSystems, newApp, runApp)
import MischiefECS.Components (Component (required))
import MischiefECS.Entities (Entity)
import MischiefECS.World.Query (query)
import MischiefECS.World.Remove (remove)
