{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lefts" #-}
{-|
Module      : ENVCAP.Manager.Manage
Description : Module is responsible for the management of project, files, and separate compilation etc.
Copyright   : (c) Jam Kabeer Ali Khan, 2025
License     : MIT
Maintainer  : jamkhan@connect.hku.hk
Stability   : experimental

For more details, see the individual function documentation.
-}
module ENVCAP.Manager.Manage where
import System.Directory ( doesFileExist, listDirectory )
import System.FilePath ((</>), takeBaseName, takeFileName)
import Control.Monad (filterM, forM_, forM)
import Data.Configurator ( load, require, Worth(Required) )
import Data.Text (pack, isSuffixOf)
import System.IO.Error 
import ENVCAP.Source.Errors 
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import ENVCAP.Manager.Implementation (Fragment, readImplementation)
import ENVCAP.Syntax
import ENVCAP.Source.Elaboration (elaborateInfer)
import Data.List (isSuffixOf) 
import System.Directory.Internal.Prelude
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode, decodeOrFail)
import ENVCAP.Interpreter (evaluate)


type ProjectName = String

-- | `getBaseDir` loads the path from environment variable `ENVCAP_CODE` in `config.cfg`
-- and returns the project path.
getBaseDir  :: IO String
getBaseDir  =   load [Required "config.cfg"] >>= \config ->
                require config (pack "ENVCAP_CODE") :: IO String

-- | `createCompiledDir` creates the compiled directory in the project directory.
createCompiledDir   :: String   -- ^ Project Name
                    -> IO ()
createCompiledDir projectName =
            do  baseDir         <- getBaseDir
                let compiledDir  = baseDir </> projectName </> "compiled"
                exists  <- doesDirectoryExist compiledDir
                unless exists $ createDirectoryIfMissing True compiledDir

-- Extracts "Variants" from "/path/to/Variants.epi"
getFileBaseName :: FilePath -> String
getFileBaseName = takeBaseName . takeFileName

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
    do  baseDir     <- getBaseDir
        filesNames  <- getFileNames (baseDir ++ projectName) `catchIOError` handleDirectoryError
        return $ map (\x -> baseDir </> projectName </> x) filesNames

-- | `getInterfaceAndImplFiles` is a utility function that returns the path of
-- all implementation and interface Files present in the specified project.
--
-- It utilizes the path specified in environment variable `ENVCAP_CODE`.
-- Reads all the files present and returns list of interface paths and list of impl paths.
getInterfaceAndImplFiles  :: ProjectName                  -- ^ Name of the project
                          -> IO ([FilePath], [FilePath])  -- ^ (interface files, implementation files)
getInterfaceAndImplFiles projName = 
    do
      allFiles <- getProjectFiles projName `catchIOError` handleDirectoryError
      return $ foldr categorize ([], []) allFiles
    where
      categorize :: FilePath -> ([FilePath], [FilePath]) -> ([FilePath], [FilePath])
      categorize file (epis, eps)
        | ".epi"  `Data.List.isSuffixOf` file = (file:epis, eps)
        | ".ep"   `Data.List.isSuffixOf` file = (epis, file:eps)
        | otherwise                           = (epis, eps)

-- | `loadProjectFiles` loads and parses the implementation files of a project.
-- It returns a list of successfully parsed fragments and a list of errors.
loadProjectFiles    :: [FilePath]   -- ^ Files to be loaded
                    -> IO ([Fragment], [ENVCAP.Source.Errors.InterpreterError])
loadProjectFiles projectFiles = do
    results         <- mapM readImplementation projectFiles
    return $ foldr categorize ([], []) results
    where
        categorize  :: Either ENVCAP.Source.Errors.InterpreterError Fragment
                    -> ([Fragment], [ENVCAP.Source.Errors.InterpreterError])
                    -> ([Fragment], [ENVCAP.Source.Errors.InterpreterError])
        categorize (Left  err)      (fragments, errors) = (fragments, err:errors)
        categorize (Right fragment) (fragments, errors) = (fragment:fragments, errors)

-- | `elaborateFragments` is a utility function that elaborates fragments to core expressions.
-- Each fragment becomes a record at the source level ~~~~> core level
--
-- === Example:
-- >>> elaborateFragments [("X", (TmLit 1))]
-- Right (Rec "X" (Lit 1))
elaborateFragments  :: [Fragment] 
                    -> Either ENVCAP.Source.Errors.SourceTypeError [(String, CoreTm)]
elaborateFragments []           = Right []
elaborateFragments fragments    = elaborateMultiple fragments
        where   elaborateMultiple :: [Fragment]   -> Either ENVCAP.Source.Errors.SourceTypeError [(String, CoreTm)]
                elaborateMultiple []        = Right []
                elaborateMultiple ((file, term):xs)    
                    =   case elaborateInfer TySUnit term of
                            Right (_, term')   ->
                                        elaborateMultiple xs >>=
                                            \xs'    ->  Right ((file, term'):xs')
                            Left err        -> Left err

-- | `saveCoreTmToFile` saves a core expression to a .epc file.
-- core expression is converted into binary file and saved in a `.epc` file.
saveCoreTmFile  :: ProjectName   
                -> (String, CoreTm) 
                -> IO (Either String ())
saveCoreTmFile projectName (fileName, coreTm)   = 
    do
        baseDir <- getBaseDir
        let filePath = baseDir </> projectName </> "compiled" </> (fileName ++ ".epc")
        result  <- try (BL.writeFile filePath (encode coreTm)) :: IO (Either SomeException ())
        case result of
            Left exception  -> return $ Left (show exception)
            Right _         -> return $ Right ()

-- | `loadCoreTmFile` loads a CoreTm expression from a .epc file.
loadCoreTmFile  :: ProjectName
                -> String 
                -> IO (Either String CoreTm)
loadCoreTmFile projectName fileName = 
    do  baseDir       <- getBaseDir
        let filePath  = baseDir </> projectName </> "compiled" </> (fileName ++ ".epc")
        fileExists    <- doesFileExist filePath
        if not fileExists
          then return $ Left ("File not found: " ++ filePath)
          else do
            result  <-  try (BL.readFile filePath) :: IO (Either SomeException BL.ByteString)
            case result of
              Left exception      -> return $ Left (show exception)
              Right byteString    ->
                case decodeOrFail byteString of
                  Left    (_, _, errMsg)  -> return $ Left ("Decoding error: " ++ errMsg)
                  Right   (_, _, coreTm)  -> return $ Right coreTm

-- | `compiledProject` creates a compiled directory in the project
-- if it doesn't exists and then, loads the project files, elaborates to
-- core expression and generates `.epc` files in the `compiled` directory.
compileProject  :: ProjectName 
                -> IO (Either [String] ())
compileProject projectName = do
  createCompiledDir projectName
  projectFiles              <- getProjectFiles projectName
  (fragments, _)  <- loadProjectFiles projectFiles

  case elaborateFragments fragments of
    Left typeError -> return $ Left [show typeError]
    Right coreTms  -> do
                        results         <- mapM (saveCoreTmFile projectName) coreTms
                        let saveErrors  = [ errMsg | Left errMsg <- results ]
                        if null saveErrors
                            then return $ Right ()
                            else return $ Left saveErrors

-- | `executeProject` loads the `.epc` files from the compiled directory in the
-- project if `/compiled` dir exists, and then, executes.
executeProject  :: ProjectName 
                -> IO ()
executeProject projectName = do
  baseDir <- getBaseDir
  let projectDir = baseDir </> projectName
  let compiledDir = projectDir </> "compiled"

  fileNames <- getFileNames projectDir

  let epcFileNames = map (\fn -> takeBaseName fn ++ ".epc") fileNames
  epcFileExists <- forM epcFileNames $ \epcFileName ->
    doesFileExist (compiledDir </> epcFileName)

  unless (and epcFileExists) $ do
    hPutStrLn stderr "Not all .epc files found in the compiled directory." 
    return ()

  forM_ epcFileNames $ \epcFileName -> do
    let baseFileName = takeBaseName epcFileName
    putStrLn $ "-----running " ++ baseFileName ++ ".epc------"
    coreTmResult <- loadCoreTmFile projectName baseFileName
    case coreTmResult of
      Left loadError -> hPutStrLn stderr $ "Error loading " ++ epcFileName ++ ": " ++ loadError
      Right coreTm -> do
        case ENVCAP.Interpreter.evaluate coreTm of
          Left evalError -> hPutStrLn stderr $ "Error evaluating " ++ epcFileName ++ ": " ++ show evalError
          Right value    -> print value  


-- | `handleDirectoryError` is an error handling function
-- for handling the case when the project directory is not found
-- in the `getProjectFiles` function.
handleDirectoryError    :: IOError       -- ^ IOError raised
                        -> IO [FilePath] -- ^ List of file names (returns [])
handleDirectoryError e
    | isDoesNotExistError e = return []
    | otherwise             = return []
