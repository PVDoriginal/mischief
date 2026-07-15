{- HLINT ignore "Use newtype instead of data" -}
module MischiefECS.App.Plugins where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable
import MischiefECS.Collectable
import MischiefECS.Components.Bundle
import MischiefECS.Components.Common
import MischiefECS.World
import Unsafe.Coerce

class (Typeable p, Eq p) => Plugin p where
  plugins :: p -> Plugins
  plugins _ = collect ()

  init :: p -> System ()
  init _ = pure ()

data ErasedPlugin where
  ErasedPlugin :: (Plugin p, Eq p) => p -> ErasedPlugin

getInit :: ErasedPlugin -> System ()
getInit (ErasedPlugin p) = MischiefECS.App.Plugins.init p

newtype Plugins = Plugins {inner :: [ErasedPlugin]} deriving newtype (Semigroup, Monoid)

instance (Plugin p) => EraseIntoStorage p Plugins where
  erase p = Plugins [ErasedPlugin p]

instance {-# OVERLAPPING #-} EraseIntoStorage () Plugins where
  erase _ = Plugins []

newtype PluginData = PluginData {inner :: Map TypeRep ErasedPlugin}

instance Show PluginData where
  show p = show $ map fst $ Map.toList p.inner

addErasedRec :: ErasedPlugin -> PluginData -> PluginData
addErasedRec (ErasedPlugin (plugin :: p)) d =
  case Map.lookup (typeOf plugin) d.inner of
    Nothing -> foldr addErasedRec (PluginData $ Map.insert (typeOf plugin) (ErasedPlugin plugin) d.inner) (plugins plugin).inner
    Just other ->
      if plugin == unsafeCoerce other
        then d
        else undefined

plugAll :: (Plugin p) => p -> PluginData
plugAll p = addErasedRec (ErasedPlugin p) (PluginData Map.empty)

runPluginRec :: (Plugin p) => p -> System ()
runPluginRec p = for_ (map snd $ Map.toList (plugAll p).inner) getInit
