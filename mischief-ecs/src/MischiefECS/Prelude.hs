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
    newApp,
    runApp,
    addSystem,
    addPlugin,
    Name (Name),
    ComponentResult (value),
  )
where

import MischiefECS.App (Plugin, addPlugin, addSystem, newApp, runApp)
import MischiefECS.Components (Component (required))
import MischiefECS.Components.Default (require)
import MischiefECS.Entities (Entity)
import MischiefECS.Tables (ComponentResult (value))
import MischiefECS.World (Name (Name), System, defer, despawn, flush, insert, insertNew, set, spawn)
import MischiefECS.World.Query (Queryable, modify, query)
import MischiefECS.World.Remove (remove)
