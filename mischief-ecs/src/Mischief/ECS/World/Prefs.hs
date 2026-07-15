module Mischief.ECS.World.Prefs where

newtype WorldPrefs = WorldPrefs
  { supressEvents :: Bool
  }

newPrefs :: WorldPrefs
newPrefs =
  WorldPrefs
    { supressEvents = False
    }

supressEvents :: Bool -> WorldPrefs -> WorldPrefs
supressEvents b WorldPrefs {} = WorldPrefs {supressEvents = b}
