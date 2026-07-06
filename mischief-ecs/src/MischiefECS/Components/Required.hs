{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.Components.Required (require, requireAll, toBundleElement) where

import Data.Data
import Data.Default
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Components

class RequiredBundle b where
  defaultBundleData :: Proxy b -> Set DefaultComponentType

instance RequiredBundle () where
  defaultBundleData :: Proxy () -> Set DefaultComponentType
  defaultBundleData _ = Set.empty

instance {-# OVERLAPPABLE #-} (Component c, Default c) => RequiredBundle c where
  defaultBundleData :: Proxy c -> Set DefaultComponentType
  defaultBundleData _ = Set.singleton $ DefaultComponentType $ Proxy @c

instance {-# OVERLAPPING #-} (RequiredBundle b0, RequiredBundle b1) => RequiredBundle (b0, b1) where
  defaultBundleData _ =
    let b0 = defaultBundleData (Proxy @b0)
        b1 = defaultBundleData (Proxy @b1)
     in Set.union b0 b1

require :: forall b. (RequiredBundle b) => Set DefaultComponentType
require = defaultBundleData (Proxy @b)

requireAll :: forall c. (Component c) => Set DefaultComponentType
requireAll = requireAll' $ required @c

requireAll' :: Set DefaultComponentType -> Set DefaultComponentType
requireAll' set =
  let nextSet = expandAll set
   in if null nextSet then set else requireAll' $ Set.union set nextSet

expandAll :: Set DefaultComponentType -> Set DefaultComponentType
expandAll set =
  let newSet = Set.unions $ map expandOne (Set.toList set)
   in Set.difference newSet set

expandOne :: DefaultComponentType -> Set DefaultComponentType
expandOne (DefaultComponentType (_ :: (Proxy c))) = required @c

toBundleElement :: DefaultComponentType -> BundleElement
toBundleElement (DefaultComponentType (_ :: (Proxy c))) = BundleElement (ComponentRep $ ComponentType $ Proxy @c) $ ErasedComponent $ def @c