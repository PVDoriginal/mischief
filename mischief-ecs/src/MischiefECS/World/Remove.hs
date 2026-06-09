{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.World.Remove where

import Data.Data
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Events
import MischiefECS.Tables
import MischiefECS.World

class Removable c where
  removeInternal :: Proxy c -> Entity -> System ()

instance {-# OVERLAPPABLE #-} (Component c) => Removable c where
  removeInternal :: Proxy c -> Entity -> System ()
  removeInternal _ entity = do
    runEvent $ eraseEvent (OnRemove @c entity)
    removeComponentFromEntity @c entity

instance {-# OVERLAPPING #-} (Removable c0, Removable c1) => Removable (c0, c1) where
  removeInternal :: Proxy (c0, c1) -> Entity -> System ()
  removeInternal _ !entity = do
    removeInternal (Proxy @c0) entity
    removeInternal (Proxy @c1) entity

remove :: forall r. (Removable r) => Entity -> System ()
remove = removeInternal (Proxy @r)

class Delete r where
  delete :: r -> System ()

instance (Component c) => Delete (ComponentResult c) where
  delete :: ComponentResult c -> System ()
  delete result = remove @c result.entity

instance (Component c) => Delete (RelationshipResult c) where
  delete :: RelationshipResult c -> System ()
  delete result = removeRelationshipFromEntity @c result.target result.entity

-- delete :: forall c. (Removable c) => ComponentResult c -> System ()
-- delete result = remove @c result.entity