module MischiefECS(module MischiefECS.World, module MischiefECS.Entities, module MischiefECS.Bundles, module MischiefECS.Components, module MischiefECS.Query) where

import Data.IORef

import MischiefECS.World
import MischiefECS.Entities
import MischiefECS.Components
import MischiefECS.Bundles
import MischiefECS.Tables
import MischiefECS.Query

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

q3 = Query @(C1, (C1, C2))

queryResult = fillQuery q3 (e1, (e1, e2))