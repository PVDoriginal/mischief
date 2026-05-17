module MischiefECS.Query where 

import Data.Data
import MischiefECS.Components 

import Data.Set
import Data.Set qualified as Set 

import MischiefECS.World
import Data.Proxy

class QueryData qd where 
    types :: qd -> Set TypeRep

instance (Component c) => QueryData (Proxy c) where
    types :: Component c => Proxy c -> Set TypeRep
    types _ = Set.singleton . typeRep $ Proxy @c 

instance (QueryData a0, QueryData a1) => QueryData (a0, a1) where
    types :: (QueryData a0, QueryData a1) => (a0, a1) -> Set TypeRep
    types (a0, a1) = Set.union (types a0) (types a1)

class QueryDataExternal 

data Query qd = (QueryData qd) => Query qd

queryTypes :: Query qd -> Set TypeRep 
queryTypes (Query qd) = types qd  