{-|
Module      : ENVCAP.Manager.Manage
Description : Module is responsible for the management of project, files, and separate compilation etc.
Copyright   : (c) Jam Kabeer Ali Khan, 2025
License     : MIT
Maintainer  : jamkhan@connect.hku.hk
Stability   : experimental

ADD DESCRIPTION

For more details, see the individual function documentation.
-}
module ENVCAP.Manager.Manage where
import System.Directory ( doesFileExist, listDirectory )
import System.FilePath ((</>))
import Control.Monad (filterM)
import Data.Configurator ( load, require, Worth(Required) )
import Data.Text (pack)
import System.IO.Error (catchIOError, isDoesNotExistError)
import ENVCAP.Source.Errors (InterpreterError)
import ENVCAP.Manager.Implementation (Fragment, readImplementation)

type ProjectName = String

-- | `getFileNames` is a utility function that returns the file names
-- present in a specific directory.
getFileNames    :: FilePath      -- ^ Path of a file/directory
                -> IO [FilePath] -- ^ List of file names located in the path
getFileNames dirPath =
    listDirectory dirPath >>= \contents ->
        filterM (doesFileExist . (dirPath </>)) contents

-- | `getProjectFiles` is a utility function that returns the Files present in
-- the specified project.
--
-- It utilizes the path specified in environment variable `ENVCAP_CODE`.
getProjectFiles :: ProjectName      -- ^ Name of the project
                -> IO [FilePath]    -- ^ List of file names in the project
getProjectFiles projectName =
    do  config      <- load [Required "config.cfg"]
        baseDir     <- require config (pack "ENVCAP_CODE") :: IO String
        filesNames  <- getFileNames (baseDir ++ projectName) `catchIOError` handleDirectoryError
        return $ map (\x -> baseDir ++ "/" ++ projectName ++ "/" ++ x) filesNames

-- | `loadProjectFiles` loads and parses the implementation files of a project.
-- It returns a list of successfully parsed fragments and a list of errors.
loadProjectFiles    :: [FilePath]
                    -> IO ([Fragment], [InterpreterError])
loadProjectFiles projectFiles = do
    results         <- mapM readImplementation projectFiles
    return $ foldr categorize ([], []) results
    where
        categorize  :: Either InterpreterError Fragment 
                    -> ([Fragment], [InterpreterError])
                    -> ([Fragment], [InterpreterError])
        categorize (Left  err)      (fragments, errors) = (fragments, err:errors)
        categorize (Right fragment) (fragments, errors) = (fragment:fragments, errors)

-- Basic step first
--
-- | Need a function that could load both implementation and project files
-- 1. Extract the project files         [DONE]
-- 2. Load the implementation files     [DONE]
-- 3. Type-check the implementation files of the project/elaborate
-- 4. Type-check at the core level once more
-- 5. Generate `.epc` files
-- 6. Create a directory compiled and put these `.epc` files there
compileProject :: ProjectName -> IO()
compileProject projectName =
    do  projectFiles        <- getProjectFiles projectName
        (fragments, errors) <- loadProjectFiles projectFiles
        putStrLn $ "\nSuccessfully parsed files in project `" ++ projectName ++ "`:"
        mapM_ (\(fileName, _)   -> putStrLn $ " - " ++ fileName) fragments

        putStrLn "\nErrors encountered:"
        mapM_ (putStrLn . (" - " ++) . show) errors

-- | Simple testing function (To be taken out)
testProjectFiles :: String -> IO ()
testProjectFiles projectName = do
    compileProject projectName


-- | `handleDirectoryError` is an error handling function
-- for handling the case when the project directory is not found
-- in the `getProjectFiles` function.
handleDirectoryError    :: IOError       -- ^ IOError raised
                        -> IO [FilePath] -- ^ List of file names (returns [])
handleDirectoryError e
    | isDoesNotExistError e = return []
    | otherwise             = return []