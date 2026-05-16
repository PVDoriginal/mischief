module MischiefECS(module MischiefECS.World, module MischiefECS.Entities, module MischiefECS.Bundles, module MischiefECS.Components) where

import Data.IORef

import MischiefECS.World
import MischiefECS.Entities
import MischiefECS.Components
import MischiefECS.Bundles
import MischiefECS.Tables
import Unsafe.Coerce (unsafeCoerce)
import Data.Data (Typeable)
import Data.Type.Equality
import Data.Typeable

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data C1 = C1 Int deriving (Component, Show) 
data C2 = C2 String deriving (Component, Show) 
data C3 = C3 Double deriving (Component, Show) 

e1 = ErasedComponent $ C1 10
e2 = ErasedComponent $ C2 "lol"
e3 = ErasedComponent $ C3 2.5  

class Erased e 
instance Erased ErasedComponent 
instance (Erased a0, Erased a1) => Erased (a0, a1) 

class (Typeable c, Erased e) => Recoverable c e where 
    recover :: e -> Maybe c

instance (Component c) => Recoverable c ErasedComponent where
    recover :: Component c => ErasedComponent -> Maybe c
    recover e = tryGetComponent c e   

instance (Recoverable r0 a0, Recoverable r1 a1) => Recoverable (r0, r1) (a0, a1) where  
    recover :: (Recoverable r0 a0, Recoverable r1 a1) => (a0, a1) -> Maybe (r0, r1)
    recover (erased0, erased1) = do 
        component1 <- recover erased0 
        component2 <- recover erased1 
        return (component1, component2)

test :: Maybe (C1, (C1, C2))
test = recover (e1, (e1, e2))

