module MischiefECS.World where

import Data.Data
import Data.Map
import Data.IORef

import MischiefECS.Entities
import MischiefECS.Components

data World = World {
    archetypes :: Archetypes,
    components :: Components, 
    entities :: Entities
} 

newWorld :: IO(World)
newWorld = do  
    archetypes <- emptyArchetypes
    components <- emptyComponents 
    entities <- emptyEntities
    let world = World archetypes components entities
    return world 

spawnEntity :: (Typeable a) => [a] -> World -> IO(Entity)
spawnEntity components world = do 
    undefined 