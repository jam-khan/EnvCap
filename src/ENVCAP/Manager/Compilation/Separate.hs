module ENVCAP.Manager.Compilation.Separate where
import ENVCAP.Manager.Manage (ProjectName, getProjectFiles)
import ENVCAP.Source.Errors (SeparateCompilationError (SepCompError))
import System.FilePath (takeExtension, takeBaseName)



checkInterfacesPresent :: ProjectName -> IO (Either SeparateCompilationError ())
checkInterfacesPresent projname = do
    projectFiles        <- getProjectFiles projname
    let epFiles         = filter (\file -> takeExtension file == ".ep") projectFiles
    let epiFiles        = filter (\file -> takeExtension file == ".epi") projectFiles

    let epBaseNames     = map takeBaseName epFiles
    let epiBaseNames    = map takeBaseName epiFiles

    -- It is okay to not have an implementation file
    let missingEpi      = filter (`notElem` epiBaseNames) epBaseNames

    if null missingEpi
        then return $ Right ()
        else return $ Left $ SepCompError $ "Missing corresponding .epi: " ++ foldr (\acc r -> acc ++ " " ++ r) "" missingEpi


-- | `readProject` reads all `.ep` and `.epi` files in the project directory.
-- It returns a list of triples containing the file name and the contents of both files.
-- readProject :: ProjectName -> IO (Either SeparateCompilationError [(Name, String, String)])
-- readProject projname = do
--                 projectFiles <- getProjectFiles



-- 2.   Read all interface files
-- 3.   Check for cycles in the interface files with requirements
-- 4.   If cycles detected, then return failure
-- 5.   If no cycles detected, then
--          simply replace interfaces inside the interface with actual types
--          Now, interfaces should be completely independent. (Kind of separate compilation of interfaces)
-- 6.   Convert interface to source type
-- 7.   Now, interfaces should be ready as types
-- 8.   Take implementation file
-- 9.   Check for cycles
-- 10.  Get a topological sorting order
-- 11.  Replace all the interface representation with actual types in the IMPORTS and REQUIREMENTS
-- 12.  Directly create a Source Level Fragment
-- 13.  Elaborate each fragment into coreTm
-- 14.  Expand >>> DESUGAR >>> LocallyNameless >>> Typecheck >>> Elaboration
-- 14.  Note: If everything is done correctly, then this should be able to typecheck independently. 
--                      That's literally the whole point of separate typechecking
-- 15.  Link the compiled fragments/core expressions with a dependent merge in topological sorting order
-- 16.  Fragment at core level will become record simply after linking (This way we can syntactically differentiate)