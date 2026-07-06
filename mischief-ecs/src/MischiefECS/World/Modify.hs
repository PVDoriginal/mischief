module MischiefECS.World.Modify where

import Data.Maybe
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Prelude
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Query

--   Just res <- single @r
--   modify res f

modify :: forall c. (Queryable c, Bundle c, QueryOutput c ~ Result c) => Result c -> (c -> c) -> System ()
modify !result !f = do
  Just res <- get @c (entityOf result)
  insert (f $ value res) (entityOf res)

--     Just res <- entityQuery @c result.entity
--     insertResource (f res.value)

alter :: forall c. (Queryable c, Bundle c, QueryOutput c ~ Result c, Component c) => (Maybe c -> Maybe c) -> Entity -> System ()
alter !f !entity = do
  val <- get @c entity
  let r = f (fmap value val)

  if isNothing r && isJust val
    then
      remove @c entity
    else case r of
      Just r ->
        insert r entity
      Nothing -> return ()
