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

-- Verify all interface names match their filenames
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

-- `processInterfaceFiles` basically takes filepaths of multiple interface files
-- and processes each.
-- 
-- returns the final processed interfaces
processInterfaceFiles :: [FilePath] -> IO (Either SeparateCompilationError [ParseIntfData])
processInterfaceFiles files = do
    -- Read files (already checks .epi extension)
    contents <- readInterfaceFiles files
    -- Parse files (gets [(FilePath, ParseIntfData)])
    case parseInterfaceFiles contents of
        Left err -> return $ Left err
        Right parsed ->
            -- Verify names match filenames
            case verifyFileNames parsed of
                Nothing ->  return $ Right . map snd $ parsed  -- Return just ParseIntfData
                Just err -> return $ Left err


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
