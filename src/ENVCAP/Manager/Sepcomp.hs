{-# HLINT ignore "Use >=>" #-}
module ENVCAP.Sepcomp where
import ENVCAP.Source.Errors
import Control.Exception
import ENVCAP.Syntax
import ENVCAP.Interpreter
import Control.Monad (liftM2)
import ENVCAP.Source.Elaboration (elaborateInfer)
import Data.List (delete)
import ENVCAP.Interface

getImplementation :: String -> Either InterpreterError SourceTm
getImplementation code =
            do  surfaceAST                  <- parseCode code
                surfaceASTExpanded          <- typeAliasExpansion surfaceAST
                surfacelocallyNameLessAST   <- locallyNameless surfaceASTExpanded
                desugarSource surfacelocallyNameLessAST

-- | Reads the interface and implementation files and returns a pair of the result as `TmAnno SourceTm SourceTyp`.
readFiles :: String -> IO (Either String SourceTm)
readFiles filePath = do
    -- Read and parse the interface file
    interfaceResult <- try (readFile (filePath ++ ".epi")) :: IO (Either IOException String)
    interfaceTyp <- case interfaceResult of
        Right res -> case getInterface res of
            Right typ -> return $ Right typ
            Left err  -> return $ Left $ "Interface error: " ++ show err
        Left ioexception -> return $ Left $ "I/O error (interface): " ++ show ioexception

    -- Read and parse the implementation file
    implementationResult <- try (readFile (filePath ++ ".ep")) :: IO (Either IOException String)
    implementationTm <- case implementationResult of
        Right res -> case getImplementation res of
            Right tm -> return $ Right tm
            Left err -> return $ Left $ "Implementation error: " ++ show err
        Left ioexception -> return $ Left $ "I/O error (implementation): " ++ show ioexception

    -- Combine the results into a `TmAnno SourceTm SourceTyp`
    case liftM2 (,) interfaceTyp implementationTm of
        Right (typ, tm) -> return $ Right $ TmAnno tm typ
        Left err        -> return $ Left err

-- | Simply checks the implementation against its interface
intraCheck :: SourceTyp -> String -> IO (Either SeparateCompilationError (SourceTyp, CoreTm))
intraCheck ctx str = do
                    result <- readFiles str
                    case result of
                        Right (TmAnno tm typ)   ->
                            case elaborateInfer ctx (TmAnno tm typ) of
                                Right (sTy, cTm)    -> return $ Right (sTy, cTm)
                                Left  _             -> return $ Left $ SepCompError "Separate Type Check Failed"
                        Right _ -> return $ Left $ SepCompError "Wrong result by readFiles utility must return annotation"
                        Left _  -> return $ Left $ SepCompError "Interface doesn't matches implementation type"

-- | Applies intraCheck to multiple file paths and prints the results
-- processFiles :: [String] -> IO [Either SeparateCompilationError (SourceTyp, CoreTm)]
-- processFiles            = mapM intraCheck

-- | Applies intraCheck to multiple file paths sequentially, accumulating the SourceTyp context.
processFiles :: [String] -> IO [Either SeparateCompilationError (SourceTyp, CoreTm)]
processFiles filePaths  = processFilesHelper filePaths TySUnit  -- Start with an empty context (STUnit)

-- | Helper function for processFiles that accumulates the SourceTyp context.
processFilesHelper :: [String] -> SourceTyp -> IO [Either SeparateCompilationError (SourceTyp, CoreTm)]
processFilesHelper [] _ = return []
processFilesHelper (filePath:rest) contextType = do
    result <- intraCheck contextType filePath 
    case result of
        Right (newType, _) -> do 
            let updatedContext = TySAnd contextType newType 
            restResults <- processFilesHelper rest updatedContext
            return (result : restResults)
        Left err -> do 
            restResults <- processFilesHelper rest contextType  
            return (Left err : restResults)

-- | A function to manually provide file paths and print results
manuallyCheckFiles :: IO ()
manuallyCheckFiles = do
    putStrLn "Enter file paths (separated by spaces):"
    line <- getLine
    let filePaths = words line
    results <- processFiles filePaths
    let formattedResults = zipWith formatResult filePaths results
    mapM_ putStrLn formattedResults

-- | Formats the result of intraCheck for a single file.
formatResult :: String -> Either SeparateCompilationError (SourceTyp, CoreTm) -> String
formatResult filePath result =
    filePath ++ ": " ++
    case result of
        Right (_, _)    -> "OK"
        Left  err       -> "Error: " ++ show err

-- A graph is represented as an adjacency list
type Graph = [(Int, [Int])]

-- Topological sort using Kahn's algorithm
topologicalSort :: Graph -> Maybe [Int]
topologicalSort graph
  | null nodesWithNoIncomingEdges = Nothing  -- Cycle detected
  | otherwise = Just sorted
  where
    -- Find all nodes with no incoming edges
    nodesWithNoIncomingEdges = [node | (node, _) <- graph, not (any (node `elem`) (map snd graph))]

    -- Remove a node from the graph and return the updated graph
    removeNode :: Int -> Graph -> Graph
    removeNode node = map (\(n, edges) -> (n, delete node edges)) . filter ((/= node) . fst)

    -- Recursively perform the topological sort
    sortHelper :: [Int] -> Graph -> [Int]
    sortHelper sorted [] = sorted
    sortHelper sorted g =
      case nodesWithNoIncomingEdges of
        [] -> []  -- Cycle detected
        (node:rest) ->
          let newGraph = removeNode node g
              newSorted = sorted ++ [node]
          in sortHelper newSorted newGraph

    sorted = sortHelper [] graph

-- Example usage
main :: IO ()
main = do
  let graph = [(1, [2, 3]), (2, [4]), (3, [4]), (4, [])]
  print $ topologicalSort graph