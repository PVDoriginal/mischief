module MischiefECS.Query where 

import Data.Data
import MischiefECS.Components 

import Data.Set
import Data.Set qualified as Set 

import MischiefECS.World
import Data.Proxy

class QueryData qd where 
    types :: qd -> Set TypeRep

instance {-# OVERLAPPABLE #-} (Component c) => QueryData (Proxy c) where
    types :: Component c => Proxy c -> Set TypeRep
    types _ = Set.singleton . typeRep $ Proxy @c 

instance (QueryData (Proxy a0), QueryData (Proxy a1)) => QueryData (Proxy (a0, a1)) where
    types :: (QueryData (Proxy a0), QueryData (Proxy a1)) => Proxy (a0, a1) -> Set TypeRep
    types _ = Set.union (types $ Proxy @a0) (types $ Proxy @a1)  

data Query qd = (QueryData (Proxy qd)) => Query qd

queryTypes :: forall qd . (QueryData (Proxy qd)) => Query qd -> Set TypeRep 
queryTypes _ = types $ Proxy @qd  

fillQuery :: (Recoverable c e) => (c -> Query c) -> e -> Maybe c  
fillQuery _ e = recover e   