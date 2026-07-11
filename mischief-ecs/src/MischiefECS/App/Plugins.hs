{- HLINT ignore "Use newtype instead of data" -}
module MischiefECS.App.Plugins where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable
import MischiefECS.World
import Unsafe.Coerce

class (Typeable p, Eq p) => Plugin p where
  plugins :: p -> Plugins
  plugins _ = Plugins []

  init :: p -> System ()
  init _ = pure ()

data ErasedPlugin where
  ErasedPlugin :: (Plugin p, Eq p) => p -> ErasedPlugin

getInit :: ErasedPlugin -> System ()
getInit (ErasedPlugin p) = MischiefECS.App.Plugins.init p

newtype Plugins = Plugins {inner :: [ErasedPlugin]}

plug :: (Plugin p) => p -> Plugins
plug p = Plugins [ErasedPlugin p]

(>.) :: (Plugin p) => Plugins -> p -> Plugins
(>.) (Plugins l) p = Plugins $ ErasedPlugin p : l

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
runPluginRec p =
  let ps = plugAll p
   in for_ (map snd $ Map.toList ps.inner) $ \x -> getInit x