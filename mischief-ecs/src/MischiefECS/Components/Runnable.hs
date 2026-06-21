{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module MischiefECS.Components.Runnable where

import Data.Data
import Data.Kind
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.World

class Runnable c where
  runFor' :: Proxy c -> (forall d. (Component d, Bundle d) => Proxy d -> System ()) -> System ()

instance {-# OVERLAPPABLE #-} (Component c, Bundle c) => Runnable c where
  runFor' c s = s c

instance {-# OVERLAPPING #-} (Runnable r0, Runnable r1) => Runnable (r0, r1) where
  runFor' _ s = do
    runFor' (Proxy @r0) s
    runFor' (Proxy @r1) s

runFor :: forall c. (Runnable c) => (forall (d :: Type). (Component d, Bundle d) => Proxy d -> System ()) -> System ()
runFor = runFor' (Proxy @c)
