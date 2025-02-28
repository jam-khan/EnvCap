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

type ProjectName = String

-- | `getFileNames` is a utility function that returns the file names
-- present in a specific directory.
getFileNames 
    :: FilePath         -- ^ Path of a file/directory
    -> IO [FilePath]    -- ^ List of file names located in the path
getFileNames dirPath =
    listDirectory dirPath >>= \contents ->
        filterM (doesFileExist . (dirPath </>)) contents

-- | `getProjectFiles` is a utility function that returns the Files present in
-- the specified project.
--
-- It utilizes the path specified in environment variable `ENVCAP_CODE`.
getProjectFiles 
    :: ProjectName      -- ^ Name of the project
    -> IO [FilePath]    -- ^ List of file names in the project
getProjectFiles projectName =
    do  config      <- load [Required "config.cfg"]
        baseDir     <- require config (pack "ENVCAP_CODE") :: IO String
        getFileNames (baseDir ++ projectName) `catchIOError` handleDirectoryError

-- | `handleDirectoryError` is an error handling function
-- for handling the case when the project directory is not found
-- in the `getProjectFiles` function.
handleDirectoryError 
    :: IOError          -- ^ IOError raised
    -> IO [FilePath]    -- ^ List of file names (returns [])
handleDirectoryError e
    | isDoesNotExistError e = return []
    | otherwise             = return []

-- | Simple testing function (To be taken out)
testProjectFiles :: String -> IO ()
testProjectFiles projectName = do
    files   <-  getProjectFiles projectName
    putStrLn $ "\nFiles in project `" ++ projectName ++ "`:\n"
    mapM_ putStrLn files 