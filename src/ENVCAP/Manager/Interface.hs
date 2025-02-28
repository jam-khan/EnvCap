{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ENVCAP.Manager.Interface where
import ENVCAP.Source.Errors
import ENVCAP.Parser.Interface.ParseInterface (parseInterface)
import ENVCAP.Syntax
import ENVCAP.Source.TypeExpansion (expandTyAlias, expandAliasTypParams)
import ENVCAP.Source.Desugar (getFixpointType, desugarTyp)

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
                =   case expandAliasTypParams tyGamma params of
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