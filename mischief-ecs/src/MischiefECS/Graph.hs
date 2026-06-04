{- HLINT ignore "Use second" -}
module MischiefECS.Graph where

import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec

data Graph a = Graph {nodes :: IORef (Map a Int), counter :: IORef Int, edges :: IOVec (Maybe a, [Int])}

newGraph :: forall a. IO (Graph a)
newGraph = do
  nodes <- newIORef Map.empty
  counter <- newIORef 0
  edges <- Vec.new 16

  return Graph {nodes, counter, edges}

addNode :: (Ord a) => a -> Graph a -> IO Int
addNode node Graph {nodes, counter, edges} = do
  index <- readIORef counter
  modifyIORef' counter (+ 1)
  Vec.pushBack edges (Just node, [])
  modifyIORef' nodes (Map.insert node index)
  return index

getOrAddNode :: (Ord a) => a -> Graph a -> IO Int
getOrAddNode node graph = do
  nodes' <- readIORef graph.nodes
  case Map.lookup node nodes' of
    Just x -> return x
    Nothing -> addNode node graph

addEdge :: (Ord a) => (a, a) -> Graph a -> IO ()
addEdge (a, b) graph = do
  a' <- getOrAddNode a graph
  b' <- getOrAddNode b graph
  Vec.modify_ graph.edges b' (\(x, l) -> (x, a' : l))

takeRemoveableNodes :: IOVec (Maybe a, [Int]) -> IO [a]
takeRemoveableNodes edges = do
  edgeList <- Vec.toList edges
  len <- Vec.length edges
  let nodes = concatMap unwrap (filter (\((a, x), _) -> null x && isJust a) (zip edgeList [0 :: Int ..]))
  for_ nodes $ \(_, i) -> Vec.modify_ edges i (\(_, l) -> (Nothing, l))
  for_ nodes $ \(_, i) -> for_ [0 .. len - 1] $ \j -> Vec.modify_ edges j (\(a, l) -> (a, List.delete i l))
  return (map fst nodes)
  where
    unwrap ((Just a, _), i) = [(a, i)]
    unwrap _ = []

getNodes :: Graph a -> IO [[a]]
getNodes Graph {edges} = do
  edges' <- Vec.clone edges
  res <- newIORef []
  step res edges'
  readIORef res
  where
    step res edges' = do
      nodes <- takeRemoveableNodes edges'
      unless (null nodes) $ do
        modifyIORef' res (++ [nodes])
        step res edges'
