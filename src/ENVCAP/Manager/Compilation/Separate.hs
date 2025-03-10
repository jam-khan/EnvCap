module ENVCAP.Manager.Compilation.Separate where
import ENVCAP.Manager.Manage (ProjectName, getProjectFiles, getBaseDir)
import ENVCAP.Source.Errors (SeparateCompilationError (SepCompError))
import System.FilePath (takeExtension, takeBaseName,(</>))
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import ENVCAP.Manager.Implementation (FileName, Path, getNameFromPath)
import Control.Exception (SomeException(SomeException), IOException, try)
import System.Directory (doesFileExist)
import ENVCAP.Syntax (SecurityLevel, Imports, Requirement, Requirements, SurfaceTm, SourceTyp)
import ENVCAP.Parser.Implementation.ParseImp (parseImplementation)
import Data.Either (rights, lefts)

type Fragment   = (FileName, SecurityLevel, Imports, Requirements, SurfaceTm)
type Interfaces = [String]

parseInterfacesFromImports :: Path -> Imports -> IO (Either SeparateCompilationError Interfaces)
parseInterfacesFromImports dirPath imports = do
    let interfacePaths  = map (\importName -> dirPath </> importName ++ ".epi") imports
    interfaceContents   <- mapM safeReadFile interfacePaths

    let errors          = lefts interfaceContents
    if not (null errors)
        then return $ Left $ SepCompError $ "Error reading interface files: " ++ show errors
        else do
                let interfaces = rights interfaceContents
                return $ Right interfaces
    where
    -- Helper function for safe file reading (same as before, but repeated for completeness)
        safeReadFile :: FilePath -> IO (Either String String)
        safeReadFile path = do
            result <- try (readFile path) :: IO (Either SomeException String)
            case result of
                Left exception  -> return $ Left (show exception)
                Right content   -> return $ Right content

    -- All imports must have interfaces
    -- We need to extract each interface from imports
{--
    data Requirement        = Implicit String String | Explicit String SurfaceTyp
                        deriving (Eq, Show)

--}
resolveInterface    :: String   -- ^ Interface file
                    -> IO (Either SeparateCompilationError SourceTyp) -- ^ Either Error or Resolved Interface
resolveInterface interface  
    = do    case parseInterface interface of
                Just (security, reqs, interface')   -> 
                    -- Requirements should become types
                    -- Implicit requirement will become a type
                    -- 


-- Compile per file
readImplementation  :: Path -- ^ Path for implementation file
                    -> IO (Either SeparateCompilationError Fragment) -- ^ Either Interpreter Failed Error or Fragment
readImplementation filePath = do
    result <- try (readFile filePath) :: IO (Either IOException String)
    case result of
        Right code ->
            case parseImplementation code of
                Just (sec, imp, req, tm)    -> return $ Right (getNameFromPath filePath, sec, imp, req, tm)
                Nothing                     -> return $ Left $ SepCompError ("Parsing Implementation File Path Failed: " ++ filePath)
        Left ioException -> return $ Left $ SepCompError ("I/O error (" ++ filePath ++ "):\n" ++ show ioException)

-- Get project name and file name
-- It should return the compiled expression with dependencies
compileFile :: ProjectName -> FileName -> IO (Either SeparateCompilationError ())
compileFile projname filename =
    do
        baseDir'            <- getBaseDir
        let implFilePath    = baseDir' </> projname </> (filename ++ ".ep")
        implFileExists      <- doesFileExist implFilePath

        if not implFileExists
            then    return $ Left $ SepCompError ("File doesn't exists: " ++ (filename ++ ".ep"))
            else    do
                        fragment    <- readImplementation implFilePath
                        putStrLn    $ show fragment
                        return      $ Right ()

-- checkInterfacesPresent :: ProjectName -> IO (Either SeparateCompilationError ())
-- checkInterfacesPresent projname = do
--     projectFiles        <- getProjectFiles projname
--     let epFiles         = filter (\file -> takeExtension file == ".ep") projectFiles
--     let epiFiles        = filter (\file -> takeExtension file == ".epi") projectFiles

--     let epBaseNames     = map takeBaseName epFiles
--     let epiBaseNames    = map takeBaseName epiFiles

--     -- It is okay to not have an implementation file
--     let missingEpi      = filter (`notElem` epiBaseNames) epBaseNames

--     if null missingEpi
--         then return $ Right ()
--         else return $ Left $ SepCompError $ "Missing corresponding .epi: " ++ foldr (\acc r -> acc ++ " " ++ r) "" missingEpi

-- type ParsedImplementation   = String
-- type ParsedInterface        = String
-- -- | `loadFileContents` checks for the implementation and loads associated interface files.
-- loadImplementation  :: ProjectName 
--                     -> FileName
--                     -> IO (Either SeparateCompilationError (ParsedImplementation, [ParsedInterface]))
-- loadImplementation projName fileName = do
--     baseDir'            <- getBaseDir
--     let implFilePath    = baseDir' </> projName </> (fileName ++ ".ep")
--     implFileExists      <- doesFileExist implFilePath

--     if not implFileExists
--         then return $ Left $ SepCompError $ "Implementation file not found: " ++ implFilePath
--         else do
--             implContent <- readFile implFilePath

-- --             -- Find and read interface files.  Gets all .epi files, then filters
-- --             -- to those that match the base name of the implementation file.
-- --             projectFiles <- getProjectFiles projName
-- --             let interfaceFiles = filter (\f -> takeExtension f == ".epi" && takeBaseName f == fileName) projectFiles

-- --             -- Read all interface files.  Use mapM and handle potential IO errors.
-- --             interfaceContents <- mapM safeReadFile interfaceFiles

-- --             -- Check for any read errors.  If any occurred, return a SepCompError.
-- --             let errors = [errMsg | Left errMsg <- interfaceContents]
-- --             if not (null errors)
-- --                 then return $ Left $ SepCompError $ "Error reading interface files: " ++ show errors
-- --                 else do
-- --                     -- Successfully read all interfaces.  Extract the Right values.
-- --                     let interfaces = [content | Right content <- interfaceContents]
-- --                     return $ Right (implContent, interfaces)
-- --   where
-- --     -- Helper function for safe file reading with error handling
-- --     safeReadFile :: FilePath -> IO (Either String String)
-- --     safeReadFile path = do
-- --       result <- try (readFile path) :: IO (Either SomeException String)
-- --       case result of
-- --         Left exception  -> return $ Left (show exception)
-- --         Right content   -> return $ Right content
--             -- First, I parse the implementation
--             -- Write the section where we formulate a path using 
--             --      getBaseDir/ProjectName/FileName.ep
--             -- This will get the path
--             -- Read this file content using readFile
--             -- then I will parse simply finish the instructions given first


-- -- | `loadProjectFilesContents` checks if all the interface files are present
-- -- then, loads and parses the files
-- -- Wrong approach: It should be able to load the interface files regardless 
-- -- of whether implementation is present or not
-- loadProjectFilesContents :: ProjectName
--                          -> IO (Either SeparateCompilationError [(String, String, String)])
-- loadProjectFilesContents projectName = do
--     sepCompCheck <- checkInterfacesPresent projectName
--     case sepCompCheck of
--         Left err -> return $ Left err
--         Right _ -> do
--             projectFiles <- getProjectFiles projectName
--             let epFiles  = sortBy (comparing takeBaseName) $ filter (\file -> takeExtension file == ".ep") projectFiles
--             let epiFiles = sortBy (comparing takeBaseName) $ filter (\file -> takeExtension file == ".epi") projectFiles

--             let groupedEpFiles  = groupBy (\a b -> takeBaseName a == takeBaseName b) epFiles
--             let groupedEpiFiles = groupBy (\a b -> takeBaseName a == takeBaseName b) epiFiles

--             epContents  <- mapM (mapM readFile) groupedEpFiles
--             epiContents <- mapM (mapM readFile) groupedEpiFiles

--             -- Use zipWith3 to combine the three lists
--             let combined = zipWith3 combineContents (map (takeBaseName . head) groupedEpFiles) epContents epiContents
--             return $ Right combined
--         where
--         -- combineContents now takes the base name, a list of EP contents, and a list of EPI contents
--         combineContents :: String -> [String] -> [String] -> (String, String, String)
--         combineContents baseName epContent epiContent = (baseName, head epContent, head epiContent)

-- -- What do I need to do?
-- -- We need to compile individually instead of compile level
-- --      That means take the projectname and file
-- --      First, parse this file
-- --      

-- -- 2.   Read all interface files
-- -- 3.   Check for cycles in the interface files with requirements
-- -- 4.   If cycles detected, then return failure
-- -- 5.   If no cycles detected, then
-- --          simply replace interfaces inside the interface with actual types
-- --          Now, interfaces should be completely independent. (Kind of separate compilation of interfaces)
-- -- 6.   Convert interface to source type
-- -- 7.   Now, interfaces should be ready as types
-- -- 8.   Take implementation file
-- -- 9.   Check for cycles
-- -- 10.  Get a topological sorting order
-- -- 11.  Replace all the interface representation with actual types in the IMPORTS and REQUIREMENTS
-- -- 12.  Directly create a Source Level Fragment
-- -- 13.  Elaborate each fragment into coreTm
-- -- 14.  Expand >>> DESUGAR >>> LocallyNameless >>> Typecheck >>> Elaboration
-- -- 14.  Note: If everything is done correctly, then this should be able to typecheck independently. 
-- --                      That's literally the whole point of separate typechecking
-- -- 15.  Generate .epc files instead of linking. (It should simply contain the core expression)
-- -- 16.  Why is linking important to happen later than before?
-- ---             We need to make sure that we can compile files one at a time
-- --              Rather than compiling all the way
-- --      Do this later
-- --   The compile project should simply compile all files with the help of another function
-- --   -- What does it needs?
-- --   -- 1. Parse the implementation
-- --      2. Load the interfaces needed for this implementation (Interface for itself and interface of imports and requirement)
-- --      3. Formulate a fragment
-- --      4. Elaborate to Core
-- --      5. Generate .epc files
-- -- 15.  Link the compiled fragments/core expressions with a dependent merge in topological sorting order
-- -- 16.  Fragment at core level will become record simply after linking (This way we can syntactically differentiate)