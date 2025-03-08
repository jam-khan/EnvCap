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
import ENVCAP.Source.Errors (SeparateCompilationError(SepCompError))

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
expandInterface tyGamma (IFragment sec reqs interface) 
            = IFragment sec reqs <$> expandInterface tyGamma interface
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
-- If it is a fragment then I have to read the interface extract the type and then, return it.
-- Make sure there are no cycles in the interface or anything.
interfaceToTyp :: Interface -> Either SeparateCompilationError SourceTyp
interfaceToTyp (IFragment sec reqs interface)
-- Take into account requirements
-- It should be a Sig
-- FIX IT LATER
                = interfaceToTyp interface

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


-- | `getInterface` is a utility function that parses the interface
-- and desugars into a type.
--
-- === Example:
-- >>> getInterface "val x : Int"
-- Right (TySRecord "x" TySInt)
getInterface :: String -> Either SeparateCompilationError SourceTyp
getInterface code = 
    case parseInterface code of
        Just interface ->
                expandInterface STUnit interface >>=
                    \expanded   -> interfaceToTyp expanded
        _   -> Left $ SepCompError "Interface parsing failed."