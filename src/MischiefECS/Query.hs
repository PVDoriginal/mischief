module MischiefECS.Query where 

import MischiefECS.World
import MischiefECS.Components 
import MischiefECS.Entities

import Data.Data


import Data.Set
import Data.Set qualified as Set 

import Data.Proxy

class QueryData qd where 
    types :: qd -> Set TypeRep

instance {-# OVERLAPPABLE #-} (Component c) => QueryData (Proxy c) where
    types :: Component c => Proxy c -> Set TypeRep
    types _ = Set.singleton . typeRep $ Proxy @c 

instance (QueryData (Proxy a0), QueryData (Proxy a1)) => QueryData (Proxy (a0, a1)) where
    types :: (QueryData (Proxy a0), QueryData (Proxy a1)) => Proxy (a0, a1) -> Set TypeRep
    types _ = Set.union (types $ Proxy @a0) (types $ Proxy @a1)  

data Query qd = (QueryData (Proxy qd)) => Query 

queryTypes :: forall qd . (QueryData (Proxy qd)) => Query qd -> Set TypeRep 
queryTypes _ = types $ Proxy @qd  

class (QueryData (Proxy qd)) => Queryable qd where 
    runQueryEntity :: Query qd -> World -> Entity -> IO (Maybe qd) 
    runQueryInternal :: Query qd -> [ArchetypeId] -> World -> IO [qd]

instance {-# OVERLAPPABLE #-} (Component c) => Queryable c where
    runQueryEntity :: Component c => Query c -> World -> Entity -> IO (Maybe c)
    runQueryEntity _ world entity = tryGetEntityComponent c world entity 

    runQueryInternal :: Component c => Query c -> [ArchetypeId] -> World -> IO [c]
    runQueryInternal _ archetypes world = tryGetComponents c world archetypes 

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1) => Queryable (q0, q1) where 
    runQueryEntity :: (Queryable q0, Queryable q1) => Query (q0, q1) -> World -> Entity -> IO (Maybe (q0, q1))
    runQueryEntity _ world entity = do 
        r0 <- runQueryEntity (Query @q0) world entity 
        r1 <- runQueryEntity (Query @q1) world entity 
        
        let res = do 
                r0 <- r0
                r1 <- r1 
                return (r0, r1)

        return res 
    
    runQueryInternal :: (Queryable q0, Queryable q1) => Query (q0, q1) -> [ArchetypeId] -> World -> IO [(q0, q1)]
    runQueryInternal _ archetypes world = do 
        r0 <- runQueryInternal (Query @q0) archetypes world 
        r1 <- runQueryInternal (Query @q1) archetypes world 
        
        return $ zip r0 r1 

runQuery :: forall qd. (QueryData (Proxy qd), Queryable qd) => Query qd -> World -> IO [qd] 
runQuery query world = 
    do 
        components <- mapM (\c -> getComponentId c world.components) (Set.toList (types (Proxy @qd)))
        archetypes <- findMatchingArchetypes components world.archetypes 
        runQueryInternal query archetypes world 

query :: forall qd -> (QueryData (Proxy qd), Queryable qd) => World -> IO [qd]
query queryData world =
    runQuery (Query @queryData) world
