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

fillQuery :: (Recoverable c e) => Query c -> e -> Maybe c  
fillQuery _ e = recover e   

class (QueryData (Proxy qd)) => Queryable qd where 
    queryEntity :: Query qd -> World -> Entity -> IO (Maybe qd) 

instance {-# OVERLAPPABLE #-} (Component c) => Queryable c where
    queryEntity :: Component c => Query c -> World -> Entity -> IO (Maybe c)
    queryEntity _ world entity = tryGetEntityComponent c world entity 

instance (Queryable q0, Queryable q1) => Queryable (q0, q1) where 
    queryEntity :: (Queryable q0, Queryable q1) => Query (q0, q1) -> World -> Entity -> IO (Maybe (q0, q1))
    queryEntity _ world entity = do 
        r0 <- queryEntity (Query @q0) world entity 
        r1 <- queryEntity (Query @q1) world entity 
        
        let res = do 
                r0 <- r0
                r1 <- r1 
                return (r0, r1)

        return res 
    