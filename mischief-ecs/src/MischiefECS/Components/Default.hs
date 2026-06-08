{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components.Default (require) where

import Data.Data
import Data.Default
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Components.Internal

class DefaultBundle b where
  defaultBundleData :: Proxy b -> DefaultBundleData

instance DefaultBundle () where
  defaultBundleData :: Proxy () -> DefaultBundleData
  defaultBundleData _ = DefaultBundleData Set.empty

instance {-# OVERLAPPABLE #-} (Component c, Default c) => DefaultBundle c where
  defaultBundleData :: Proxy c -> DefaultBundleData
  defaultBundleData _ =
    let DefaultBundleData other = required @c
     in DefaultBundleData $ Set.union (Set.fromList [BundleElement {rep = ComponentRep $ typeRep (Proxy @c), component = ErasedComponent $ def @c}]) other

instance {-# OVERLAPPING #-} (DefaultBundle b0, DefaultBundle b1) => DefaultBundle (b0, b1) where
  defaultBundleData :: Proxy (b0, b1) -> DefaultBundleData
  defaultBundleData _ =
    let DefaultBundleData set0 = defaultBundleData $ Proxy @b0
        DefaultBundleData set1 = defaultBundleData $ Proxy @b1
     in DefaultBundleData $ Set.union set0 set1

require :: forall b. (DefaultBundle b) => DefaultBundleData
require = defaultBundleData (Proxy @b)
