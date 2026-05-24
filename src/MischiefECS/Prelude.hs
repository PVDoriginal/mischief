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
    defer,
    set,
    modify,
    flush,
    System,
    Plugin,
    Startup (Startup),
    Update (Update),
    newApp,
    runApp,
    addSystem,
    addPlugin,
    Name (Name),
  )
where

import MischiefECS.App (Plugin, Startup (Startup), Update (Update), addPlugin, addSystem, newApp, runApp)
import MischiefECS.Components (Component (required))
import MischiefECS.Components.Default (require)
import MischiefECS.Entities (Entity)
import MischiefECS.World (Name (Name), System, defer, despawn, flush, insert, insertNew, modify, set, spawn)
import MischiefECS.World.Query (Queryable, query)
import MischiefECS.World.Remove (remove)
