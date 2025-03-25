{-|
Module      : ENVCAP.Manager.Interface
Description : Module is responsible for parsing and loading of multiple interface files.
Copyright   : (c) Jam Kabeer Ali Khan, 2025
License     : MIT
Maintainer  : jamkhan@connect.hku.hk
Stability   : experimental

Key functionalities:
- Functionality 1: Parse interface files.
- Functionality 2: Structure interface files.

For more details, see the individual function documentation.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ENVCAP.Manager.Interface where
import ENVCAP.Source.Errors
import ENVCAP.Parser.Interface.ParseInterface (parseInterface)
import ENVCAP.Syntax
import ENVCAP.Source.TypeExpansion (expandTyAlias, expandAliasTypParams)
import ENVCAP.Source.Desugar (getFixpointType, desugarTyp)
import Control.Exception
import System.FilePath ((</>))

-- | `expandInterface` is a utility function that performs type alias expansion
-- on interface files.
--
-- === Example:
--
-- >>> expandInterface STUnit (InterfaceAnd (IAliasTyp "INT" STInt) (Binding "x" (STIden "INT")))
-- Right (Binding "x" STInt)
expandInterface :: SurfaceTyp   -- ^ Typing context for type expansion.
                -> Interface    -- ^ Interface to be expanded.
                -> Either SeparateCompilationError Interface    -- ^ Returns error or expanded interface.
expandInterface _ (IAliasTyp name ty)   =
    Left $ SepCompError ("Type Alias: type " ++ name ++ " = " ++ show ty ++ " not expanded properly.")

expandInterface tyGamma (IType     ty)  =
    case expandTyAlias tyGamma ty of
        Right ty'   -> Right    $ IType ty'
        Left  _     -> Left     $ SepCompError ("Type " ++ show ty ++ "did not expand properly.")

expandInterface tyGamma (FunctionTyp name params ty)=
    case expandAliasTypParams tyGamma params of
        Right params'  ->
            case expandTyAlias tyGamma ty of
                Right ty'   -> Right $ FunctionTyp name params' ty'
                Left  _   ->
                    Left  $ SepCompError
                            ("Interface Type Expansion Failed during output type expansion of function " ++ name)
        Left _    ->
                Left  $ SepCompError
                            ("Interface Type Expansion Failed during the params expansion of function " ++ name)

expandInterface tyGamma (ModuleTyp name params ty)=
    case expandAliasTypParams tyGamma params of
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

expandInterface tyGamma (Binding name ty)=
    case expandTyAlias tyGamma ty of
        Right ty'   -> Right $ Binding name ty'
        Left _      -> Left  $ SepCompError ("Interface Type Expansion Failed during the type expansion of binding " ++ name)

expandInterface tyGamma (InterfaceAnd stmt1 stmt2)=
    case stmt1 of
        (IAliasTyp name ty) ->
            case expandTyAlias tyGamma ty of
                Right ty'   ->
                    expandInterface (STAnd tyGamma (STRecord name ty')) stmt2
                Left  _     ->
                    Left $ SepCompError ("Interface Type Expansion Failed when expanding type inside the alias " ++ name)
        _       -> InterfaceAnd <$> expandInterface tyGamma stmt1 <*> expandInterface tyGamma stmt2


-- | `interfaceToTyp` is a utility function to desugar interface
-- to a type.
--
-- Note: Types and Interfaces are unified.
--
-- === Example:
-- >>> interfaceToTyp (IType (STInt))
-- Right TySInt
interfaceToTyp :: Interface -> Either SeparateCompilationError SourceTyp
interfaceToTyp (IAliasTyp _name _ty)
                =   Left (SepCompError "Type Alias detected at interface desugaring stage (Should not be possible) if expansion done correctly.")
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

-- | `getModuleInputType` is a utility function that
-- desugars the parameters of a module to a type.
--
-- === Example:
-- >>> getModuleInputType [("num", STInt)]
-- Right TySInt
getModuleInputType :: Params -> Either DesugarError SourceTyp
getModuleInputType []           =
            Left  $ DesugarFailed "Module must have at least one parameter!"
getModuleInputType [(_, ty)]    =
            desugarTyp ty
getModuleInputType ((_, ty):xs) =
            TySAnd <$> desugarTyp ty <*> getModuleInputType xs


-- | `getRequirements` is a utility function that takes
-- requirements and makes all implicits into explicits.
-- 
-- === Example:
-- >>> getRequirements ([Implicit "Lib" "Lib"])
-- [Explicit "Lib" (TySRecord "inc" (TySArrow TySInt TySInt)]
getRequirements :: Requirements -> IO (Either SeparateCompilationError [(String, SourceTyp)])
getRequirements [] = return $ Right []
getRequirements (req:rest) = do
    restResult <- getRequirements rest
    case restResult of
        Left err -> return $ Left err
        Right rest' -> case req of
            Implicit name interface -> do
                {-- Fix the hardcoded path later --}
                fullInterfaceResult <- readInterface ("examples/Source/SepComp1" </> (interface ++ ".epi"))
                case fullInterfaceResult of
                    Left err -> return $ Left err
                    Right typ -> return $ Right $ (name, typ) : rest'
            Explicit name typ -> do
                case desugarTyp typ of
                    Left err -> return $ Left $ SepCompError (show err)
                    Right typ' -> return $ Right $ (name, typ') : rest'


convertRequirementsToTyp :: [(String, SourceTyp)] -> Either SeparateCompilationError SourceTyp
convertRequirementsToTyp []                 = Left $ SepCompError "Failed"
convertRequirementsToTyp [(name, ty)]       = Right $ TySRecord name ty
convertRequirementsToTyp ((name, ty):rest)  =
    case convertRequirementsToTyp rest of
        Right rest'     -> return $ TySAnd (TySRecord name ty) rest'
        Left err        -> Left err


-- | `processRequirements` is a utility function that processes a list of requirements
-- by first converting them to explicit requirements using `getRequirements` and then
-- converting the result to a `SourceTyp` using `convertRequirementsToTyp`.
--
-- === Example:
-- >>> processRequirements [Implicit "Lib" "Lib"]
-- Right (TySRecord "Lib" (TySArrow TySInt TySInt))
processRequirements :: Requirements -> IO (Either SeparateCompilationError SourceTyp)
processRequirements ls = do
    requirementsResult <- getRequirements ls
    case requirementsResult of
        Left err            -> return $ Left err
        Right requirements  -> return $ convertRequirementsToTyp requirements


-- | `getFullInterface` is a utility function that takes
-- requirements and makes all implicit into explicit.
--
-- === Example:
-- >>> getFullInterface "examples/Source/SepComp1/Utils.epi"
-- Right (TySSig (TySRecord "Lib" (TySRecord "inc" (TySArrow TySInt TySInt))) (TySRecord "apply" (TySArrow TySInt TySInt)))
readInterface :: String -> IO (Either SeparateCompilationError SourceTyp)
readInterface file =
    do  result <- try (readFile file) :: IO (Either IOException String)
        print result
        process result
    where 
        process result = 
            case result of
                Left _      -> return $ Left $ SepCompError "Interface file not found"
                Right code  -> parse code
        
        parse result = case parseInterface result of
                    Just (_auth, requirements, interface)   ->
                        f requirements interface
                    Nothing     -> return $ Left $ SepCompError "Failed to parse"

        f requirements interface
                = if not (null requirements)
                    then
                        processReq
                    else
                        processEmptyReq 
            where   
                processReq =
                    do  reqs <- processRequirements requirements
                        case reqs of
                            Right reqs' ->
                                case expandInterface STUnit interface of
                                    Right expanded  -> 
                                        case interfaceToTyp expanded of
                                            Right expanded' ->
                                                return $ Right (TySSig reqs' expanded')
                                            Left _      ->
                                                return $ Left (SepCompError "Interface to Type conversion failed")
                                    Left err        -> return $ Left err
                            Left err    -> return $ Left err
                        where 
                processEmptyReq =
                    case expandInterface STUnit interface of
                        Right expanded  -> return $ interfaceToTyp expanded
                        Left err        -> return $ Left err
