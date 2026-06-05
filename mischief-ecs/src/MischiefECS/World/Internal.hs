module MischiefECS.World.Internal where

import MischiefECS.World

data ParWorld = ParWorld {world :: World, deferred :: IO [System ()]}
