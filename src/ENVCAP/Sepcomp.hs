{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
module ENVCAP.Sepcomp where
import ENVCAP.Source.Errors
import Control.Exception
import ENVCAP.Parser.Interface.ParseInterface (parseInterface)
import ENVCAP.Syntax
import ENVCAP.Source.TypeExpansion (expandTyAlias, expandAliasTypParams)
import ENVCAP.Source.Desugar (getFixpointType, desugarTyp)
import ENVCAP.Interpreter
import Control.Monad (liftM2)
import System.Environment (getArgs)
import ENVCAP.Source.Elaboration (elaborateInfer)



-- Step 1: Add annotations                                          [Done]
-- Step 2: Read interface files                                     [Done]
-- Step 3: Read implementation files                                [Done]
-- Step 4: Combine implementation and interface as an annotation    [Done]


-- Step 5: Read multiple files in order                             [~2 hours]
--          Order reflects dependencies: File1 File2 (means that File2 depends on File1)
--          Individually typecheck these files
--          Iteratively, build the typing context
--          Elaborate each file and simply release new files .epc

-- Step 6:  Iterative merge these .epc                              [~2 hours]
--          Evaluate all of these .epc files
--          Execute and get the result


-- Step 6: Add three commands: 
--                  SeparateTypeCheck: File 1 ... File N (Check all files against header files)
--                  SeparateCompile:   File 1 ... File N (  1) Check 
--                                                          2) Load modules in environment as a box
--                                                          3) Elaborate to core and write in a file
--                                                          4) Each file gets a core generation
--                  envcap "E1.epc" ~~~~> Evaluates result
--

-- | Performs type expansion on the interface
--  Returns `Right Interface AST` or SepCompError if couldn't parse interface correctly
interfaceTypeExpansion :: SurfaceTyp -> Interface -> Either SeparateCompilationError Interface
interfaceTypeExpansion _ (IAliasTyp name ty)  =
                        Left  $ SepCompError ("Type Alias: type "
                            ++ name ++ " = " ++ show ty
                            ++ " not expanded properly.")
interfaceTypeExpansion tyGamma (IType     ty)
                =   case expandTyAlias tyGamma ty of
                        Right ty'   -> Right    $ IType ty'
                        Left  _     -> Left     $ SepCompError ("Type " ++ show ty ++ "did not expand properly")
interfaceTypeExpansion tyGamma (FunctionTyp name params ty)
                =  case expandAliasTypParams tyGamma params of
                        Right params'  ->
                            case expandTyAlias tyGamma ty of
                                Right ty'   -> Right $ FunctionTyp name params' ty'
                                Left  _   ->
                                    Left  $ SepCompError
                                            ("Interface Type Expansion Failed during output type expansion of function " ++ name)
                        Left _    ->
                                Left  $ SepCompError
                                            ("Interface Type Expansion Failed during the params expansion of function " ++ name)
interfaceTypeExpansion tyGamma (ModuleTyp name params ty)
                =   case expandAliasTypParams tyGamma params of
                        Right params'   ->
                            case expandTyAlias tyGamma ty of
                                Right ty'   ->
                                    Right $ ModuleTyp name params' ty'
                                Left _    ->
                                    Left $ SepCompError
                                            ("Interface Type Expansion Failed during output type expansion of module " ++ name)
                        Left _        ->
                            Left $ SepCompError
                                            ("Interface Type Expansion Failed during the params expansion of module " ++ name)
interfaceTypeExpansion tyGamma (Binding name ty)
                =   case expandTyAlias tyGamma ty of
                        Right ty'   -> Right $ Binding name ty'
                        Left _      -> Left  $ SepCompError ("Interface Type Expansion Failed during the type expansion of binding " ++ name)
interfaceTypeExpansion tyGamma (InterfaceAnd stmt1 stmt2)
                =   case stmt1 of
                        (IAliasTyp name ty) ->
                                        case expandTyAlias tyGamma ty of
                                            Right ty'   ->
                                                interfaceTypeExpansion (STAnd tyGamma (STRecord name ty')) stmt2
                                            Left  _     ->
                                                Left $ SepCompError ("Interface Type Expansion Failed when expanding type inside the alias " ++ name)
                        _       -> InterfaceAnd <$> interfaceTypeExpansion tyGamma stmt1 <*> interfaceTypeExpansion tyGamma stmt2

-- | Performs desugaring of interface to a type
interfaceToTyp :: Interface -> Either SeparateCompilationError SourceTyp
interfaceToTyp (IAliasTyp _namee _ty)
                =   Left (SepCompError "Type Aliase detected at interface desugaring stage (Should not be possible) if expansion done correctly.")
interfaceToTyp (IType ty)
                =   case desugarTyp ty of
                        Right ty'                   ->  Right ty'
                        Left (DesugarFailed err)    ->
                            Left $ SepCompError ("Interface Type Expansion Failed " ++ err)
interfaceToTyp (FunctionTyp name params ty)
                =   case desugarTyp ty of
                        Right ty'   ->
                            case getFixpointType params ty' of
                                Right ty''                  -> Right $ TySRecord name ty''
                                Left  (DesugarFailed err)   -> Left  $ SepCompError ("Interface Type Expansion Failed " ++ err)
                        Left (DesugarFailed err) ->
                            Left $ SepCompError ("Interface Type Expansion Failed " ++ err)
interfaceToTyp (ModuleTyp name params ty)
                =   case getModuleInputType params of
                        Right tyA   ->
                            case desugarTyp ty of
                                Right tyB   -> Right $ TySRecord name (TySSig tyA tyB)
                                Left err    -> Left  $ SepCompError ("Interface Type Expansion Failed " ++ show err)
                        Left err    -> Left $ SepCompError ("Interface Type Expansion Failed" ++ show err)
interfaceToTyp (Binding name ty)
                =   case desugarTyp ty of
                        Right ty'   -> Right $ TySRecord name ty'
                        Left  err   -> Left $ SepCompError ("Interface Type Expansion Failed" ++ show err)
interfaceToTyp (InterfaceAnd ty1 ty2)
                =   TySAnd <$> interfaceToTyp ty1 <*> interfaceToTyp ty2


getModuleInputType :: Params -> Either DesugarError SourceTyp
getModuleInputType []           =
            Left  $ DesugarFailed "Module must have at least one parameter!"
getModuleInputType [(_, ty)]    =
            desugarTyp ty
getModuleInputType ((_, ty):xs) =
            TySAnd <$> desugarTyp ty <*> getModuleInputType xs

-- | Parses the interface file
--  Returns `Right Interface AST` or SepCompError if couldn't parse interface correctly.
readInterface :: String -> Either SeparateCompilationError Interface
readInterface file = case parseInterface file of
                        Just interface  ->  Right interface
                        Nothing         ->  Left $ SepCompError "Couldn't read the interface loaded."

getInterface :: String -> Either SeparateCompilationError SourceTyp
getInterface code = readInterface code >>= \res ->
                        interfaceTypeExpansion STUnit res >>=
                            \expanded   -> interfaceToTyp expanded

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

-- -- | Main function that takes file paths as command-line arguments.
-- main :: IO ()
-- main = do
--   args <- getArgs
--   if null args
--     then putStrLn "Please provide file paths as arguments."
--     else processFiles args
main :: IO ()
main = manuallyCheckFiles