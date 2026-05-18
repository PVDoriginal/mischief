module MischiefECS.Query where 

import MischiefECS.World
import MischiefECS.Components 
import MischiefECS.Entities

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep, TypeRep)
import Data.Set
import Data.Set qualified as Set 

class QueryData qd where 
    types :: Proxy qd -> Set TypeRep

instance {-# OVERLAPPABLE #-} (Component c) => QueryData c where
    types :: Component c => Proxy c -> Set TypeRep
    types = Set.singleton . typeRep

instance (QueryData a0, QueryData a1) => QueryData (a0, a1) where
    types :: (QueryData a0, QueryData a1) => Proxy (a0, a1) -> Set TypeRep
    types _ = Set.union (types $ Proxy @a0) (types $ Proxy @a1)  

class (QueryData qd) => Queryable qd where 
    runQueryEntity :: Proxy qd -> World -> Entity -> IO (Maybe qd) 
    runQueryInternal :: Proxy qd -> [ArchetypeId] -> World -> IO [qd]

instance {-# OVERLAPPABLE #-} (Component c) => Queryable c where
    runQueryEntity :: Component c => Proxy c -> World -> Entity -> IO (Maybe c)
    runQueryEntity _ world entity = tryGetEntityComponent c world entity 

    runQueryInternal :: Component c => Proxy c -> [ArchetypeId] -> World -> IO [c]
    runQueryInternal _ archetypes world = tryGetComponents c world archetypes 

instance {-# OVERLAPPING #-} (Queryable q0, Queryable q1) => Queryable (q0, q1) where 
    runQueryEntity :: (Queryable q0, Queryable q1) => Proxy (q0, q1) -> World -> Entity -> IO (Maybe (q0, q1))
    runQueryEntity _ world entity = do 
        r0 <- runQueryEntity (Proxy @q0) world entity 
        r1 <- runQueryEntity (Proxy @q1) world entity 
        
        -- let res = do 
        --         r0 <- r0
        --         r1 <- r1 
        --         return (r0, r1)
        -- here you can actually do (,) <$> r0 <*> r1 using Maybe's applicative and functor instance
        -- or the slightly more readable (IMO) version
        -- liftA2 imo reads nicer but it's only upto 2 values, so if you had more you'd have to use the (,) <$> a1 <*> a2 <*> ... <*> aN version
        -- P.S.: liftA2 f r0 r1 = do { a <- r0; b <- r1; pure $ f a b }
        return $ liftA2 (,) r0 r1
      
    
    runQueryInternal :: (Queryable q0, Queryable q1) => Proxy (q0, q1) -> [ArchetypeId] -> World -> IO [(q0, q1)]
    runQueryInternal _ archetypes world = do 
        r0 <- runQueryInternal (Proxy @q0) archetypes world 
        r1 <- runQueryInternal (Proxy @q1) archetypes world 
        
        return $ zip r0 r1 

runQuery :: forall qd. (QueryData qd, Queryable qd) => Proxy qd -> World -> IO [qd] 
runQuery query world = 
    do 
        components <- mapM (\c -> getComponentId c world.components) (Set.toList (types query))
        archetypes <- findMatchingArchetypes components world.archetypes 
        runQueryInternal query archetypes world 

query :: forall qd. (QueryData qd, Queryable qd) => World -> IO [qd]
query world =
    runQuery (Proxy @qd) world
