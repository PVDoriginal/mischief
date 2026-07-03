module MischiefECS.World.Modify where

import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Prelude
import MischiefECS.Tables
import MischiefECS.World.Insert
import MischiefECS.World.Query

--   Just res <- single @r
--   modify res f

modify :: forall c. (Queryable c, Bundle c, QueryOutput c ~ ComponentResult c) => ComponentResult c -> (c -> c) -> System ()
modify !result !f = do
  Just res <- get @c (entityOf result)
  insert (f $ value res) (entityOf res)

--     Just res <- entityQuery @c result.entity
--     insertResource (f res.value)
