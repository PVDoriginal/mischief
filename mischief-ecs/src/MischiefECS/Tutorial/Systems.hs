module MischiefECS.Tutorial.Systems
  ( -- * System ()
    -- $system()
    module MischiefECS.World,
  )
where

import MischiefECS.App
import MischiefECS.App.Scheduler
import MischiefECS.App.Schedules
import MischiefECS.World

-- $system()
-- A 'System' in Mischief is simply a 'Monad' allowing operations to be executed on a 'World'.
--
-- Here is a simple 'System' that spawns an 'Entity' and prints its index:
-- @
-- foo :: System ()
-- foo = do
--
-- @