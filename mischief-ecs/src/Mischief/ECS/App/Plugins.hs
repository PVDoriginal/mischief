{-# LANGUAGE AllowAmbiguousTypes #-}

{- HLINT ignore "Use newtype instead of data" -}
module Mischief.ECS.App.Plugins (Plugin (..), Dependency, dep, addPluginRec) where

import Control.Monad
import Data.Foldable
import Data.Kind
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Mischief.ECS.Components
import Mischief.ECS.Components.Common
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Spawn

class (Typeable p) => Plugin (p :: Type) where
  deps :: [Dependency]
  deps = []

  init :: System ()
  init = pure ()

data Dependency where
  Dependency :: (Plugin p) => Proxy p -> Dependency

instance Eq Dependency where
  (Dependency p) == (Dependency p') = typeRep p == typeRep p'

instance Ord Dependency where
  compare (Dependency p) (Dependency p') = compare (typeRep p) (typeRep p')

instance Show Dependency where
  show (Dependency p) = show (typeRep p)

dep :: forall p. (Plugin p) => Dependency
dep = Dependency (Proxy @p)

data PluginMarker p = PluginMarker deriving (Component)

addPluginRec :: forall p. (Plugin p) => System ()
addPluginRec = addPluginRec' @p Set.empty

addPluginRec' :: forall p. (Plugin p) => Set Dependency -> System ()
addPluginRec' set = do
  x <- query $ mkQuery' E (With (C @(PluginMarker p)))

  when (null x) $ do
    when (Set.member (Dependency (Proxy @p)) set) $ error $ "Cyclic Plugin Dependency: " ++ show set

    for_ (Mischief.ECS.App.Plugins.deps @p) $ \(Dependency (_ :: Proxy p')) -> do
      addPluginRec' @p' (Set.insert (Dependency (Proxy @p)) set)

    void $ spawn (Name . show . typeRep $ Proxy @p, PluginMarker @p)
    Mischief.ECS.App.Plugins.init @p
