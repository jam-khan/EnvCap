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

-- Valid DAG
getNodes :: Graph -> [FilePath]
getNodes graph = S.toList $ S.unions (M.keysSet graph : map S.fromList (M.elems graph))

validGraph :: Graph
validGraph = M.fromList
  [ ("A", ["B", "C"])
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