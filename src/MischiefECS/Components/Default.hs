{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components.Default where

import Data.Data
import Data.Default
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Components.Bundle

class DefaultBundle b where
  defaultBundleData :: Proxy b -> DefaultBundleData

instance DefaultBundle () where
  defaultBundleData :: Proxy () -> DefaultBundleData
  defaultBundleData _ = DefaultBundleData Set.empty

instance {-# OVERLAPPABLE #-} (Component c, Default c) => DefaultBundle c where
  defaultBundleData :: (Component c, Default c) => Proxy c -> DefaultBundleData
  defaultBundleData c = DefaultBundleData (Set.fromList [BundleElement {rep = typeRep c, component = ErasedComponent $ def @c}])

instance {-# OVERLAPPING #-} (DefaultBundle b0, DefaultBundle b1) => DefaultBundle (b0, b1) where
  defaultBundleData :: (DefaultBundle b0, DefaultBundle b1) => Proxy (b0, b1) -> DefaultBundleData
  defaultBundleData _ =
    let DefaultBundleData set0 = defaultBundleData $ Proxy @b0
        DefaultBundleData set1 = defaultBundleData $ Proxy @b1
     in DefaultBundleData $ Set.union set0 set1

require :: forall b. (DefaultBundle b) => DefaultBundleData
require = defaultBundleData (Proxy @b)
