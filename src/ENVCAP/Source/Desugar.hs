{-# LANGUAGE LambdaCase #-}
module ENVCAP.Source.Desugar where
import ENVCAP.Syntax
import ENVCAP.Source.Errors



extractLetParams :: [(String, SurfaceTyp, SurfaceTm)] -> [(String, SurfaceTyp)]
extractLetParams [] = []
extractLetParams ((x, ty, _tm):rest)
                    = (x, ty) : extractLetParams rest

extractLetArgs :: [(String, SurfaceTyp, SurfaceTm)] -> [SurfaceTm]
extractLetArgs []   = []
extractLetArgs ((_x, _ty, tm):rest)
                    = tm : extractLetArgs rest

processModule :: SurfaceTm -> Either DesugarError SourceTm
processModule (SStruct params tm) = TmStruct <$> processParams params <*> desugar tm
processModule _ = Left $ DesugarFailed "Utility to process modules called on a non-module"

processParams :: Params -> Either DesugarError SourceTyp
processParams []        = Left $ DesugarFailed "Empty Params not allowed in the module struct"
processParams [(_, ty)] = desugarTyp ty
processParams ((_, ty):rest) = TySAnd <$> desugarTyp ty <*> processParams rest


processApplication :: SurfaceTm -> Either DesugarError SourceTm
processApplication (SApp tm1 args)
    = do
        tm1' <- desugar tm1
        args' <- mapM desugar args
        return $ foldl TmApp tm1' args'
processApplication tm
    = Left $ DesugarFailed ("Utility function called on non-application: " ++ show tm)

processLambda :: SurfaceTm -> Either DesugarError SourceTm
processLambda (SLam [] tm)          = desugar tm
processLambda (SLam (arg:rest) tm)  =
    TmLam <$> desugarParam arg <*> processLambda (SLam rest tm)
processLambda _                     =
    Left $ DesugarFailed "Utility function `processLambda` called upon non-lambda term."

desugarParam :: (String, SurfaceTyp) -> Either DesugarError SourceTyp
desugarParam (_, ty) = desugarTyp ty

desugarTyp :: SurfaceTyp -> Either DesugarError SourceTyp
desugarTyp STUnit               = Right TySUnit
desugarTyp STInt                = Right TySInt
desugarTyp STBool               = Right TySBool
desugarTyp STString             = Right TySString
desugarTyp (STAnd   ty1 ty2)    =
    TySAnd    <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (STArrow ty1 ty2)    =
    TySArrow  <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (STRecord l ty)      =
    TySRecord l <$> desugarTyp ty
desugarTyp (STList ty)          =
    TySList   <$> desugarTyp ty
desugarTyp (STSum  ty1 ty2)     =
    TySSum    <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (STPair ty1 ty2)     =
    TySPair   <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (STSig ty1 ty2)      =
    TySSig    <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (STIden x)           =
    Left $ DesugarFailed
        ("Type alias not expanded correctly. Type identifier remaining: " ++ show x)
desugar :: SurfaceTm -> Either DesugarError SourceTm
desugar SCtx            = Right TmCtx
desugar SUnit           = Right TmUnit
desugar (SLit i)        = Right (TmLit i)
desugar (SBool b)       = Right (TmBool b)
desugar (SString s)     = Right (TmString s)
desugar (SLam params tm)= processLambda (SLam params tm)
desugar (SClos tm1 params tm2)
                        = desugar tm1 >>= \tm1' ->
                            processLambda (SLam params tm2) >>= \case
                                (TmLam ty tm2')   -> Right $ TmClos tm1' ty tm2'
                                _                 -> Left $ DesugarFailed "Closure must be an abstraction. Hint: Use boxes instead"
desugar (SRec l tm)     = 
    TmRec l <$> desugar tm
desugar (SRProj tm l)   = 
    TmRProj <$> desugar tm <*> Right l
desugar (SProj tm n)    = 
    TmProj  <$> desugar tm <*> Right n
desugar (SApp tm1 args) = 
    processApplication (SApp tm1 args)
desugar (SMrg tm1 tm2)  = 
    TmMrg <$> desugar tm1 <*> desugar tm2
desugar (SBox tm1 tm2)  = 
    TmBox <$> desugar tm1 <*> desugar tm2
desugar (SVar x)        = 
    Left $ DesugarFailed ("Variable identifier " ++ show x ++ " couldn't get transformed.")
desugar (SStruct params tm)
                        = processModule (SStruct params tm)
desugar (SFunc name params ty tm)
                        = do
                            ty' <- desugarTyp ty
                            tm' <- desugar (SLam params tm)
                            return $ TmRec name (TmFix ty' tm')
desugar (SModule name params tm)
                        = TmRec name <$> desugar (SStruct params tm)
desugar (SAliasTyp l _) =
                        Left $ DesugarFailed ("Type alias" ++ show l ++ "detected at desugaring phased.")
desugar (SLet letargs tm)
                        = desugar (SApp (SLam (extractLetParams letargs) tm) (extractLetArgs   letargs))
desguar (SLetrec letargs tm)
                        = desugar 
desugar _sourceTerm     =
    Left $ DesugarFailed "Function not implemented completely."
