module MischiefECS.World.Modify where

import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Prelude
import MischiefECS.Tables
import MischiefECS.World.Insert
import MischiefECS.World.Query

modifyResource :: forall r. (Component r, Queryable r, Storage r ~ ResourceStorage, QueryOutput r ~ ComponentResult r) => (r -> r) -> System ()
modifyResource f = do undefined

--   Just res <- single @r
--   modify res f

class Modify c t where
  modify :: (Storage c ~ t) => ComponentResult c -> (c -> c) -> System ()

instance (Bundle c, Queryable c, QueryOutput c ~ ComponentResult c) => Modify c ComponentStorage where
  modify :: ComponentResult c -> (c -> c) -> System ()
  modify !result !f = do
    Just res <- entityQuery @c result.entity
    MischiefECS.World.Insert.insert (f res.value) result.entity

instance (Component c, Queryable c, QueryOutput c ~ ComponentResult c, Storage c ~ ResourceStorage) => Modify c ResourceStorage where
  modify :: ComponentResult c -> (c -> c) -> System ()
  modify !result !f = do undefined

--     Just res <- entityQuery @c result.entity
--     insertResource (f res.value)
