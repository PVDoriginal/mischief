module MischiefECS.Components.Internal where

import Data.Data
import Data.Set

newtype DefaultBundleData = DefaultBundleData (Set BundleElement)

data ErasedComponent where
  ErasedComponent :: (Typeable c) => c -> ErasedComponent

data BundleElement = BundleElement {rep :: TypeRep, component :: ErasedComponent}
