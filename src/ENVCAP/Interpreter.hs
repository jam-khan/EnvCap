module ENVCAP.Interpreter where
import Control.Exception (try, IOException)
import ENVCAP.Syntax
import ENVCAP.Parser.Happy (parseSource)
import ENVCAP.Source.TypeExpansion (expandAlias)
import ENVCAP.Source.LocallyNameless
import ENVCAP.Source.Errors
import ENVCAP.Source.Desugar (desugar)
import ENVCAP.Source.Elaboration (Elab, elaborateInfer, elaborateTyp)
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.TypeChecker (check)


-- | Parses a string of code into a 'SurfaceTm' or returns an 'InterpreterError' on failure.
--
-- If 'parseSource' returns 'Just SurfaceTm', it's converted to 'Right SurfaceTm'.
-- If 'parseSource' returns 'Nothing', it's converted to 'Left (InterpreterFailed "Parsing unsuccessful.")'.
--
-- === Example:
-- >>> parseCode "1 + 2"
-- Right (SBinOp (Arith Add) (SLit 1) (SLit 2))
-- >>> parseCode "env"
-- Right SCtx
-- >>> parseCode "function 1"
-- Parse error
parseCode :: String -> Either InterpreterError SurfaceTm
parseCode code = maybe (Left $ InterpreterFailed "Parsing unsuccessful.") Right (parseSource code)

-- | Expands type aliases within the surfaceAST, returning the expanded term or an 'InterpreterError' on failure.
--
--   This function uses 'expandAlias' to replace type aliases with their definitions
--   within the given 'surfaceAST'. If successful, the expanded 'SurfaceTm' is returned
--   in a 'Right'. Otherwise, an 'InterpreterError' containing a specific error message
--   is returned in a 'Left', detailing the cause of the expansion failure.
--
-- === Example:
-- >>> typeAliasExpansion SCtx
-- Right SCtx
typeAliasExpansion :: SurfaceTm -> Either InterpreterError SurfaceTm
typeAliasExpansion surfaceAST =
    case expandAlias STUnit surfaceAST of
        Right expandedAST               -> Right expandedAST
        Left (AliasNotFound err)        -> Left  (InterpreterFailed ("Type Expansion Failed: some type alias not located. " ++ err))
        Left (DuplicateAlias err)       -> Left  (InterpreterFailed ("Type Expansion Failed: duplicate type aliases detected. " ++ err))
        Left (TypeContextError err)     -> Left  (InterpreterFailed ("Type Expansion Failed: typing context not well-defined during expansion. " ++ err))
        Left (TypeExpansionFailed err)  -> Left  (InterpreterFailed ("Type Expansion Failed: " ++ err))

-- | Converts 'SurfaceTm' to locally nameless, returning 'Right' or 'Left' 'InterpreterError'.
--
-- Errors during 'astTolocallyNameless' (scope, params, De Bruijn) produce 'InterpreterError'.
--
-- === Example:
-- >>> locallyNameless SCtx
-- Right SCtx
locallyNameless :: SurfaceTm -> Either InterpreterError SurfaceTm
locallyNameless surfaceAST =
    case astToLocallyNameless [] surfaceAST of
        Right locallyNamelessAST         -> Right locallyNamelessAST
        Left (ScopeError err)            -> Left (InterpreterFailed ("Locally Nameless Failed: scope errors detected! check scope of the functions. " ++ err))
        Left (ParamError err)            -> Left (InterpreterFailed ("Locally Nameless Failed: check function parameters. " ++ err))
        Left (DebruijnFailed err)        -> Left (InterpreterFailed ("Locally Nameless Failed: failure at deBruijn indices assignment. " ++ err))
        Left (LocallyNamelessFailed err) -> Left (InterpreterFailed ("Locally Nameless Failed: " ++ err))

-- | Desugars a 'SurfaceTm' into a 'SourceTm', or returns an 'InterpreterError' on failure.
--
--   Uses the 'desugar' function. A 'DesugarFailed' error results in a 'Left' containing an 'InterpreterError'.
--
-- === Example:
-- >>> desugarSource SCtx
-- Right TmCtx
desugarSource :: SurfaceTm -> Either InterpreterError SourceTm
desugarSource surfaceAST =
    case desugar surfaceAST of
        Right sourceAST           -> Right sourceAST
        Left  (DesugarFailed err) -> Left (InterpreterFailed ("Desugaring Failed: " ++ err))

-- | Elaborates a 'SourceTm' into a typed 'SourceTm', or returns an 'InterpreterError'.
--
--   Uses 'elaborateInfer' with initial type 'TySUnit'. A 'STypeError' during elaboration results in a 'Left' with an 'InterpreterError'.
--
-- === Example:
-- >>> elaboration TmCtx
-- Right (TySUnit,Ctx)
elaboration :: SourceTm -> Either InterpreterError Elab
elaboration sourceAST =
    case elaborateInfer TySUnit sourceAST of
        Right (sourceTy, coreAST) -> Right (sourceTy, coreAST)
        Left  (STypeError err)    -> Left (InterpreterFailed ("Elaboration Failed (Type error): " ++ err))

-- | Evaluates a 'CoreTm' to a 'Value', or 'InterpreterError' if evaluation fails.
-- 
-- Uses 'eval' (initial 'VUnit'). Assumes sound type checking.
--
-- === Example
-- >>> evaluate Ctx
-- Right VUnit
evaluate :: CoreTm -> Either InterpreterError Value
evaluate coreAST  = case eval VUnit coreAST of
                        Just val  -> Right val
                        Nothing   -> Left (InterpreterFailed ("Evaluation Failed: It must not have failed if type checking is sound." ++ show coreAST))

-- | Interprets code to a 'Value', or 'InterpreterError' if any stage fails.
-- 
-- Steps: parse, type alias expansion, locally nameless conversion, desugaring, elaboration, type checking, evaluation.
-- 
-- === Example: 
-- >>> interpreter "env"
-- Right VUnit
interpreter :: String -> Either InterpreterError Value
interpreter code =
    do  surfaceAST                  <- parseCode code
        surfaceASTExpanded          <- typeAliasExpansion surfaceAST
        surfacelocallyNameLessAST   <- locallyNameless surfaceASTExpanded
        sourceASTDesugared          <- desugarSource surfacelocallyNameLessAST
        (sourceTy, coreAST)         <- elaboration sourceASTDesugared
        if check TyCUnit coreAST (elaborateTyp sourceTy)
            then evaluate coreAST
            else Left $ InterpreterFailed "Type checking faile at core level"

-- | Runs a file by reading its contents. Handles potential I/O errors.
--
-- Reads the file at the given 'filePath'. Returns 'Right ()' if successful,
-- or a 'Left' containing an 'InterpreterError' with a message describing the I/O error.
--
-- === Example:
-- >>> runFile "examples/Source/Arithmetic.ep"
-- Right ()
-- runFile :: String -> IO (Either InterpreterError Value)
-- runFile filePath = do
--     result <- try (readFile filePath) :: IO (Either IOException String)
--     case result of
--         Left ioException ->
--           return $ Left (InterpreterFailed $ "I/O error: " ++ show ioException)
--         Right code ->  return $ interpreter code
runFile :: String -> IO()
runFile filePath = do
    result <- try (readFile filePath) :: IO (Either IOException String)
    case result of
        Left ioexception -> putStrLn ("I/O error: " ++ show ioexception)
        Right code       -> case interpreter code of
                                Right res                       -> print res
                                Left (InterpreterFailed err)    -> putStrLn err
        --   return $ Left (InterpreterFailed $ "I/O error: " ++ show ioException)
        -- Right code ->  return $ interpreter code


