module MischiefECS.World.Remove where 


class Removable c where
  removeInternal :: Proxy c -> Entity -> System ()

instance {-# OVERLAPPABLE #-} (Component c) => Removable c where
  removeInternal :: (Component c) => Proxy c -> Entity -> System ()
  removeInternal _ = removeComponentFromEntity @c

instance {-# OVERLAPPING #-} (Removable c0, Removable c1) => Removable (c0, c1) where
  removeInternal :: (Removable c0, Removable c1) => Proxy (c0, c1) -> Entity -> System ()
  removeInternal _ !entity = do
    removeInternal (Proxy @c0) entity
    removeInternal (Proxy @c1) entity

remove :: forall r. (Removable r) => Entity -> System ()
remove = removeInternal (Proxy @r)

delete :: forall c. (Removable c) => ComponentResult c -> System ()
delete result = remove @c result.entity
