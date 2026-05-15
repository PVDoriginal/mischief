module MischiefECS.World where

import Data.Data
import Data.Map
import Data.IORef

import MischiefECS.Entities
import MischiefECS.Components
import MischiefECS.Bundles

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

-- | Spawn an entity in this World given a bundle of components. 
spawnEntity :: (Bundle b) => b -> World -> IO(Entity)
spawnEntity bundle world = 
    do
        components <- getComponentIds bundle world.components 
        archetype <- MischiefECS.Components.getArchetypeId components world.archetypes 
        putStrLn $ show archetype
        return $ Entity 5  
     