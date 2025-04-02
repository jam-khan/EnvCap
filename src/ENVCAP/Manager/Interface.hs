{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ENVCAP.Manager.Interface where
import ENVCAP.Source.Errors
import ENVCAP.Manager.Manage
import ENVCAP.Parser.Interface.ParseInterface (parseInterface)
import ENVCAP.Syntax
import ENVCAP.Source.TypeExpansion (expandTyAlias, expandAliasTypParams)
import ENVCAP.Source.Desugar (getFixpointType, desugarTyp)
import Control.Exception
import System.FilePath ((</>), takeBaseName, takeFileName)
import System.IO (readFile)
import System.Directory (doesDirectoryExist, doesFileExist)
import Control.Monad (unless)
import Data.Either (rights, partitionEithers)
import Data.List
import ENVCAP.Utils (readFileSafe)
import Data.Maybe ( mapMaybe)
import ENVCAP.Manager.Sort (Graph, getDependencyOrder, getNodes)
import qualified Data.Map.Strict as M

-- `parseInterfaceFile` takes one file content and parses
-- 
-- Returns either separate compilation error or the result
parseIntfFile   :: (FilePath, String)
                -> Either SeparateCompilationError (FilePath, ParseIntfData)
parseIntfFile (path, contents) =
    case parseInterface contents of
        Just res -> Right (path, res)  -- Preserve path in success case
        Nothing  -> Left $ SepCompError ("Parse failed: " ++ path)

-- `parseInterfaceFiles` takes file contents and parses
-- each files and returns a list of parsed data.
-- 
-- returns the parsed data
parseInterfaceFiles :: [(FilePath, String)]
                    -> Either SeparateCompilationError [(FilePath, ParseIntfData)]
parseInterfaceFiles = traverse parseIntfFile

-- `readInterfaceFiles` basically takes filepaths of multiple interface files
-- and reads each with some basic checks on files.
--
-- returns the content of the files.
readInterfaceFiles  :: [FilePath]
                    -> IO [(FilePath, String)]
readInterfaceFiles files = do
    case partition (".epi" `isSuffixOf`) files of
        (validFiles, []) -> do
            contents <- mapM readFileSafe validFiles
            return $ zip validFiles (rights contents)
        (_, invalidFiles) ->
            fail $ "Invalid extensions in: " ++ show invalidFiles

-- `verifyFileNames` is a utility function that ensures that 
-- top-level interface name matches the file name
--
-- Returns Error or Nothing
verifyFileNames :: [(FilePath, ParseIntfData)] -> Maybe SeparateCompilationError
verifyFileNames parsedFiles =
    let check (path, (name, _, _, _)) =
            if name == getFileBaseName path
            then Nothing
            else Just $ SepCompError $
                 "Interface name '" ++ name ++ "' doesn't match filename '" ++ getFileBaseName path ++ "'"
    in case mapMaybe check parsedFiles of
        [] -> Nothing
        (err:_) -> Just err  -- Return first error found

-- | `buildDependencyGraph` converts the parsed interface data
-- into a dependencyGraph
-- 
-- Convert parsed interfaces into a dependency graph
buildDependencyGraph :: [ParseIntfData] -> Graph
buildDependencyGraph interfaces = 
    M.fromList $ map (\(name, _, requirements, _) -> 
        (name, extractDependencies requirements)) interfaces
    where
        -- Extract all dependencies from requirements
        extractDependencies :: Requirements -> [Name]
        extractDependencies reqs = 
            [ dep | Req _ dep <- reqs ]  -- Only take Req constructs, ignore Params

-- | `sortByDependencyOrder` simply returns the topological
--  sort of the parsed interfaces.
-- 
-- Returns list of topologically sorted interface lists
sortByDependencyOrder :: [ParseIntfData] -> [Name] -> [ParseIntfData]
sortByDependencyOrder interfaces order =
    let 
        interfaceMap = M.fromList [(name, intf) | intf@(name, _, _, _) <- interfaces]
    in
        mapMaybe (`M.lookup` interfaceMap) order

-- | `sortInterfacesTopologically` creates a dependency graph
-- performs topological sorting and detects cycles, if any. 
-- 
-- Returns the ParsedIntfData in the topological sort order
sortInterfacesTopologically :: [ParseIntfData] -> Either SeparateCompilationError [ParseIntfData]
sortInterfacesTopologically interfaces =
    let 
        graph           = buildDependencyGraph interfaces
        interfaceNames  = map (\(name, _, _, _) -> name) interfaces
        allNodes        = getNodes graph
        missingDeps     = filter (`notElem` interfaceNames) allNodes
    in  
        if      not (null missingDeps)
        then    Left $ SepCompError $ "Missing interface dependencies: " ++ show missingDeps
        else    case getDependencyOrder graph of
                    Right order -> 
                        Right (sortByDependencyOrder interfaces order)
                    Left _      -> 
                        Left $ SepCompError "Cyclic dependencies detected between interfaces"

processInterfaceFiles   :: ProjectName 
                        -> IO (Either SeparateCompilationError [ParseIntfData])
processInterfaceFiles projname = do
    -- Step 1: Read interface files
    (interfaceFiles, _) <- getInterfaceAndImplFiles projname
    if null interfaceFiles
        then    return $ Right []
        else    do  contentsWithPaths <- readInterfaceFiles interfaceFiles
                    let processingResult = 
                            do  parseWithPaths <- parseInterfaceFiles contentsWithPaths
                                case verifyFileNames parseWithPaths of
                                    Nothing     -> return () -- check this!! maybe
                                    Just err    -> Left err
                                let intfDataOnly = map snd parseWithPaths
                                sortInterfacesTopologically intfDataOnly
                    return processingResult
                
-- -- `processInterfaceFiles` basically takes filepaths of multiple interface files
-- -- and processes each.
-- -- 
-- -- returns the final processed interfaces
-- processInterfaceFiles :: [FilePath] -> IO (Either SeparateCompilationError [ParseIntfData])
-- processInterfaceFiles files = do
--     -- Read files (already checks .epi extensixon)
--     contents <- readInterfaceFiles files
--     case parseInterfaceFiles contents of
--         Left err -> return $ Left err
--         Right parsed ->
--             -- Verify names match filenames
--             case verifyFileNames parsed of
--                 Nothing ->  return $ Right . map snd $ parsed  -- Return just ParseIntfData
--                 Just err -> return $ Left err

-- Now, we have ParseIntfData



-- -- Read all the headers
-- getHeaders :: ProjectName -> IO (Either SeparateCompilationError [ParseIntfData])
-- getHeaders projName = 
--     do
--         baseDir <- getBaseDir
--         let projDir = baseDir </> projName
--         exists <- doesDirectoryExist projDir
--         unless exists $ 

-- parseHeader :: FilePath -> IO (Either SeparateCompilationError ParseIntfData)
-- parseHeader filePath = 
--     do  fileContent <- try (readFile filePath) :: IO (Either IOException String)
--         case fileContent of
--             Left  e       -> return $ Left $ SepCompError $ show e
--             Right content -> 
--                 case parseInterface content of
--                     Nothing     -> return $ Left $ SepCompError ("Interface Parsing Failed for file: " ++ filePath)
--                     Just result -> return $ Right result

-- extractRequirements :: FilePath -> Requirements -> Either SeparateCompilationError SourceRequirements
-- extractRequirements path []      = Right []
-- extractRequirements path (x:xs)  =
--     case x of
--         -- Assumption intfName is found in the current path
--         Req name intfName       -> 
--             case processHeader (FilePath </> intfName) of
--                 Right ty        -> uhhjnmkm,
--         Param name typ          -> extractRequirements xs >>= 
--                                         \xs' -> 
--                                             ((name, typ):xs')

-- -- Write Tests
-- -- 
-- ---------------------------------
-- processHeader :: ParseIntfData -> Either SeparateCompilationError SourceHeader
-- processHeader (name, auth, reqs, intf) =
--     -- 
-- -- 1st Pass
-- -- Remove Type Aliases and Expand
-- -- TBC
-- -- Test

-- -- 2nd Pass
-- -- Read other interface files
-- -- in the requirements
--     -- Perform 1st and 2nd pass for each interface file read
-- -- Test

-- -- Put interface file in the proper simplified form
-- -- desugar into source level
-- -- Test

-- -- P
