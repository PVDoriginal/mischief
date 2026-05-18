module MischiefECS (module MischiefECS.World, module MischiefECS.Entities, module MischiefECS.Bundles, module MischiefECS.Components, module MischiefECS.Query, module MischiefECS.App) where

import Data.Data (Typeable)
import Data.IORef
import Data.Type.Equality
import Data.Typeable
import MischiefECS.App
import MischiefECS.Bundles
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Query
import MischiefECS.Tables
import MischiefECS.World
import Unsafe.Coerce (unsafeCoerce)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data C1 = C1 Int deriving (Component, Show)

data C2 = C2 String deriving (Component, Show)

data C3 = C3 Double deriving (Component, Show)

e1 = ErasedComponent $ C1 10

e2 = ErasedComponent $ C2 "lol"

e3 = ErasedComponent $ C3 2.5

-- q3 = Query @(C1, (C1, C2))
