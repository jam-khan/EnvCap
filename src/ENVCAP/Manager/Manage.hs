{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : ENVCAP.Manager.Manage
Description : Module is responsible for the management of project, files, and separate compilation etc.
Copyright   : (c) Jam Kabeer Ali Khan, 2025
License     : MIT
Maintainer  : jamkhan@connect.hku.hk
Stability   : experimental

IMPROVEMENT NEEDED:
    1. Instead of using Interpreter Error. Use error for separate compilation.


For more details, see the individual function documentation.
-}
module ENVCAP.Manager.Manage where
import System.Directory ( doesFileExist, listDirectory )
import System.FilePath ((</>))
import Control.Monad (filterM)
import Data.Configurator ( load, require, Worth(Required) )
import Data.Text (pack)
import System.IO.Error (catchIOError, isDoesNotExistError)
import ENVCAP.Source.Errors 
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import ENVCAP.Manager.Implementation (Fragment, readImplementation)
import ENVCAP.Syntax
import ENVCAP.Source.Elaboration (elaborateInfer)
import System.Directory.Internal.Prelude (unless)
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode, decode, Binary, decodeOrFail)
import Control.Exception (try, SomeException (SomeException))
import GHC.Generics (Generic)


instance Binary CoreTm
instance Binary CoreTyp
instance Binary Value
instance Binary BinaryOp
instance Binary UnaryOp
instance Binary ArithOp
instance Binary CompOp
instance Binary LogicOp

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

-- | `loadProjectFiles` loads and parses the implementation files of a project.
-- It returns a list of successfully parsed fragments and a list of errors.
loadProjectFiles    :: [FilePath]
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

-- | `fragmentsToRecords` is a utility function that converts fragments to records.
--
-- === Example:
-- >>> fragmentsToRecords [("X", (TmLit 1))]
-- [TmRec "X" (TmLit 1)]
fragmentsToRecords  :: [Fragment] 
                    -> [SourceTm]
fragmentsToRecords []   = []
fragmentsToRecords ((name, fragment):xs)
    = TmRec name fragment : fragmentsToRecords xs


-- | `elaborateFragments` is a utility function that elaborates fragments to core expressions.
-- Each fragment becomes a record at the source level ~~~~> core level
--
-- === Example:
-- >>> elaborateFragments [("X", (TmLit 1))]
-- Right (Rec "X" (Lit 1))
elaborateFragments  :: [Fragment] 
                    -> Either ENVCAP.Source.Errors.SourceTypeError [CoreTm]
elaborateFragments []   = Right []
elaborateFragments fragments = elaborateMultiple (fragmentsToRecords fragments)
        where   elaborateMultiple :: [SourceTm]   -> Either ENVCAP.Source.Errors.SourceTypeError [CoreTm]
                elaborateMultiple []        = Right []
                elaborateMultiple (x:xs)    =
                    case elaborateInfer TySUnit x of
                        Right (_, x')   ->
                                    elaborateMultiple xs >>=
                                        \xs'    ->  Right (x':xs')
                        Left err        -> Left err

-- | `saveCoreTmToFile` saves a core expression to a .epc file.
--
-- A core expression is converted into binary file and saved in a `.epc` file.
saveCoreTmFile :: ProjectName   -> (String, CoreTm) -> IO (Either String ())
saveCoreTmFile projectName (fileName, coreTm)   = 
    do
        baseDir <- getBaseDir
        let filePath = baseDir </> projectName </> "compiled" </> (fileName ++ ".epc")
        result  <- try (BL.writeFile filePath (encode coreTm)) :: IO (Either SomeException ())
        case result of
            Left exception  -> return $ Left (show exception)
            Right _         -> return $ Right ()

-- | `loadCoreTmFile` loads a CoreTm expression from a .epc file.
loadCoreTmFile :: ProjectName -> String -> IO (Either String CoreTm)
loadCoreTmFile projectName fileName = 
    do
        baseDir     <- getBaseDir
        let filePath = baseDir </> projectName </> "compiled" </> (fileName ++ ".epc")
        fileExists  <- doesFileExist filePath
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



-- | `
-- Basic step first
--
-- | Need a function that could load both implementation and project files
-- 1. Extract the project files         [DONE]
-- 2. Load the implementation files     [DONE]
-- 3. Type-check the implementation files of the project/elaborate [Done]
-- 4. Type-check at the core level once more (REDUNDUNT STEP)
-- 5. Generate `.epc` files
-- 6. Create a directory compiled and put these `.epc` files there
compileProject  :: ProjectName 
                -> IO()
compileProject projectName =
    do  projectFiles        <- getProjectFiles projectName
        (fragments, errors) <- loadProjectFiles projectFiles
        putStrLn $ "\nSuccessfully parsed files in project `" ++ projectName ++ "`:"
        mapM_ (\(fileName, _)   -> putStrLn $ " - " ++ fileName) fragments
        putStrLn "\nErrors encountered:"
        mapM_ (putStrLn . (" - " ++) . show) errors
        case elaborateFragments fragments of
            Right terms     -> do   putStrLn $ "\nSuccessfully elaborated files in project `" ++ projectName ++ "`:"
                                    mapM_ (\(Rec fileName tm')   -> putStrLn $ " - " ++ fileName ++ (show tm')) terms
            Left _          -> putStrLn "Elaboration Failed"


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
