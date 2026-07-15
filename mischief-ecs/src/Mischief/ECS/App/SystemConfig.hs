module Mischief.ECS.App.SystemConfig where

import Mischief.ECS.World

data SystemConfigData = SystemConfigData
  { systems :: [System ()],
    edges :: [(System (), System ())]
  }

-- | First type will be a SystemConfig. Second type will be a System ().
data SystemConfigModifier s1 s2 = ModAfter s1 s2 | ModBefore s1 s2

after :: s1 -> System () -> SystemConfigModifier s1 (System ())
after = ModAfter

before :: s1 -> System () -> SystemConfigModifier s1 (System ())
before = ModBefore

class SystemConfig s where
  systemConfigData :: s -> SystemConfigData

instance SystemConfig (System ()) where
  systemConfigData :: System () -> SystemConfigData
  systemConfigData system = SystemConfigData {systems = [system], edges = []}

instance (SystemConfig s1) => SystemConfig (SystemConfigModifier s1 (System ())) where
  systemConfigData :: SystemConfigModifier s1 (System ()) -> SystemConfigData
  systemConfigData (ModAfter s1 s2) =
    let SystemConfigData {systems, edges} = systemConfigData s1
        newEdges = [(s2, system) | system <- systems]
     in SystemConfigData {systems, edges = newEdges ++ edges}
  systemConfigData (ModBefore s1 s2) =
    let SystemConfigData {systems, edges} = systemConfigData s1
        newEdges = [(system, s2) | system <- systems]
     in SystemConfigData {systems, edges = newEdges ++ edges}

instance (SystemConfig s0, SystemConfig s1) => SystemConfig (s0, s1) where
  systemConfigData :: (s0, s1) -> SystemConfigData
  systemConfigData (s0, s1) =
    let SystemConfigData {systems = systems0, edges = edges0} = systemConfigData s0
        SystemConfigData {systems = systems1, edges = edges1} = systemConfigData s1
     in SystemConfigData {systems = systems0 ++ systems1, edges = edges0 ++ edges1}