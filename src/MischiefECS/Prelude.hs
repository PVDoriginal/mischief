module MischiefECS.Prelude
  ( Entity,
    despawn,
    insert,
    insertNew,
    spawn,
    Component,
    required,
    require,
    Queryable,
    query,
    remove,
    System,
    Plugin,
    Startup (Startup),
    Update (Update),
    newApp,
    runApp,
    addSystem,
    addPlugin,
  )
where

import MischiefECS.App (Plugin, Startup (Startup), Update (Update), addPlugin, addSystem, newApp, runApp)
import MischiefECS.Components (Component (required))
import MischiefECS.Components.Default (require)
import MischiefECS.Entities (Entity)
import MischiefECS.World (System, despawn, insert, insertNew, spawn)
import MischiefECS.World.Query (Queryable, query)
import MischiefECS.World.Remove (remove)
