module MischiefECS.Graph where

import Data.IORef

data Graph a = Graph {nodes :: IORef [(Int, a)], edges :: IORef [(Int, Int)]}
