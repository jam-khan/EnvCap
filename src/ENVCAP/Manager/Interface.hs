{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ENVCAP.Manager.Interface where
import ENVCAP.Source.Errors
import ENVCAP.Manager.Manage
import ENVCAP.Parser.Interface.ParseInterface (parseInterface)
import ENVCAP.Syntax
import ENVCAP.Source.TypeExpansion (expandTyAlias, expandAliasTypParams)
import ENVCAP.Source.Desugar (getFixpointType, desugarTyp)
import Control.Exception
import System.FilePath ((</>))
import System.IO (readFile)
import System.Directory (doesDirectoryExist)
import Control.Monad (unless)

-- Utility function to get and verify project directory
getProjectDir :: ProjectName -> IO (Either SeparateCompilationError FilePath)
getProjectDir projName = do
    baseDir <- getBaseDir
    let projDir = baseDir </> projName
    exists <- doesDirectoryExist projDir
    pure $ if exists
        then Right projDir
        else Left $ SepCompError $ "Project directory does not exist: " ++ projDir


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
