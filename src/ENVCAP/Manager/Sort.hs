module ENVCAP.Manager.Sort where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List    (foldl')
import Data.Maybe   (fromMaybe)

-- | A graph represented as adjacency lists (Map node dependencies)
type Graph = M.Map FilePath [FilePath]


-- -- | Topological sort with cycle detection using Kahn's algorithm
-- topologicalSort :: Graph    -> Either String [FilePath]
-- topologicalSort graph   =
--     let -- Collect all nodes (keys and dependencies)
--         allNodes    = S.toList % S.unions (M.keysSet graph : map S.fromList (M.elems graph))

-- `getNodes` collects all nodes from the graph.
--
-- === Example:
-- >>> getNodes cyclicGraph
-- ["A","B","C"]
getNodes :: Graph -> [FilePath]
getNodes graph = S.toList $ S.unions (M.keysSet graph : map S.fromList (M.elems graph))

-- `reverseGraph` simply returns reversed graph
--
-- === Example:
-- >>> reverseGraph cyclicGraph
-- fromList [("A",["C"]),("B",["A"]),("C",["B"])]
--
-- >>> reverseGraph validGraph
-- fromList [("B",["A"]),("C",["B","A"])]
reverseGraph :: Graph -> Graph
reverseGraph graph  =   M.fromListWith (++)
                        [ (dep, [node]) | (node, deps) <- M.toList graph, dep <- deps]

-- `in-degree` calculates the initial in-degree of all nodes
--
-- === Example:
-- >>> 
-- inDegree :: Graph -> Integer
inDegree :: Graph -> M.Map FilePath Integer
inDegree graph = M.fromListWith (+)
                    [ (dep, 1) | deps <- M.elems graph, dep <- deps]
                    `M.union`
                    M.fromList [(n, 0) | n <- getNodes graph]



topologicalSort :: Graph -> Either String [FilePath]
topologicalSort graph =
    let
        allNodes        = getNodes graph
        reversedGraph   = reverseGraph graph
        initialQueue    = [n | (n, degree) <- M.toList (inDegree graph), degree == 0]
        -- Kahn's algorithm implementation
        process :: [FilePath]   -- ^ Work queue 
                -> [FilePath]   -- ^ Result accumulator (reversed)
                -> M.Map FilePath Integer   -- ^ Current in-degree state
                -> Either String [FilePath]
        process [] sorted inDegMap
            | length sorted     == length allNodes = Right (reverse sorted)
            | otherwise         = Left "Cyclic modules dependencies detected."
        process (n:ns) sorted inDegMap =
            let -- Get all nodes that depend on this node
                dependents              = fromMaybe [] (M.lookup n reversedGraph)
                (newInDeg, newQueue)    = foldl' (updateInDegree n) (inDegMap, ns) dependents
            in process newQueue (n:sorted) newInDeg
        updateInDegree  :: FilePath
                        -> (M.Map FilePath Integer, [FilePath])
                        -> FilePath
                        -> (M.Map FilePath Integer, [FilePath])
        updateInDegree current (inDeg, q) dep =
            let newInDeg' = M.adjust (subtract 1) dep inDeg
                newDegree = fromMaybe 0 (M.lookup dep newInDeg')
            in if newDegree == 0
                    then (newInDeg', dep : q)
                    else (newInDeg', q)
        in process initialQueue [] (inDegree graph)

validGraph :: Graph
validGraph = M.fromList
  [ ("A", ["B"])
  , ("B", ["C"])
  , ("C", [])
  ]

-- Cyclic graph
cyclicGraph :: Graph
cyclicGraph = M.fromList
  [ ("A", ["B"])
  , ("B", ["C"])
  , ("C", ["A"])
  ]

-- main :: IO ()
-- main = do
--   print $ topologicalSort validGraph
--   -- Right ["C","B","A"]
  
--   print $ topologicalSort cyclicGraph
