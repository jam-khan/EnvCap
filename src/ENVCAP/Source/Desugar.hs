module ENVCAP.Source.Desugar where
import ENVCAP.Core.Syntax as Core
import ENVCAP.Source.Syntax as Source

surfaceUnaryToCoreOp :: TmUnaryOp -> UnaryOp
surfaceUnaryToCoreOp TmNot              = Not


lookupTyp :: Source.Typ -> String -> Maybe Source.Typ
lookupTyp (Source.TAnd ty1 (Source.TRecord label' ty2)) label 
                                        = if label' == label    then Just ty2
                                                                else lookupTyp ty1 label
lookupTyp _ _                           = Nothing

expandTyAlias :: Source.Typ -> Source.Typ -> Maybe Source.Typ
expandTyAlias tyCtx Source.TUnit           = Just Source.TUnit
expandTyAlias tyCtx Source.TInt            = Just Source.TInt
expandTyAlias tyCtx Source.TBool           = Just Source.TBool
expandTyAlias tyCtx Source.TString         = Just Source.TString
expandTyAlias tyCtx (Source.TAnd ty1 ty2)  = Source.TAnd           <$> expandTyAlias tyCtx ty1  <*> expandTyAlias tyCtx ty2
expandTyAlias tyCtx (Source.TArrow tA tB)  = Source.TArrow         <$> expandTyAlias tyCtx tA   <*> expandTyAlias tyCtx tB
expandTyAlias tyCtx (Source.TRecord l ty)  = Source.TRecord l      <$> expandTyAlias tyCtx ty
expandTyAlias tyCtx (Source.TList ty)      = Source.TList          <$> expandTyAlias tyCtx ty
expandTyAlias tyCtx (Source.TSum  ty1 ty2) = Source.TSum           <$> expandTyAlias tyCtx ty1  <*> expandTyAlias tyCtx ty2
expandTyAlias tyCtx (Source.TPair ty1 ty2) = Source.TPair          <$> expandTyAlias tyCtx ty1  <*> expandTyAlias tyCtx ty2
expandTyAlias tyCtx (Source.TSig  tyA tyB) = Source.TSig           <$> expandTyAlias tyCtx tyA  <*> expandTyAlias tyCtx tyB
expandTyAlias tyCtx (Source.TIden label)   = lookupTyp tyCtx label

expandAlias :: Source.Typ -> Tm -> Maybe Tm 
expandAlias _      TmCtx                      = Just TmCtx
expandAlias _      TmUnit                     = Just TmUnit
expandAlias _     (TmLit n)                   = Just $ TmLit n
expandAlias _     (TmBool b)                  = Just $ TmBool b
expandAlias _     (TmString s)                = Just $ TmString s
expandAlias tyCtx (TmAliasTyp label typ)      = Nothing -- This should not be reached during expansion!
expandAlias tyCtx (TmBinOp op tm1 tm2)        = case (expandAlias tyCtx tm1, expandAlias tyCtx tm2) of
                                                        (Just tm1', Just tm2') -> Just (TmBinOp op tm1' tm2')
                                                        _                       -> Nothing
expandAlias tyCtx (TmUnOp op tm)              = TmUnOp op       <$> expandAlias tyCtx tm
expandAlias tyCtx (TmIf tm1 tm2 tm3)          = TmIf            <$> expandAlias tyCtx tm1 <*> expandAlias tyCtx tm2 <*> expandAlias tyCtx tm3
expandAlias tyCtx (TmFix tm)                  = TmFix           <$> expandAlias tyCtx tm
expandAlias tyCtx (TmMrg tm1 tm2)             = case tm1 of 
                                                        (TmAliasTyp label typ)  -> expandAlias (Source.TAnd tyCtx (Source.TRecord label typ)) tm2
                                                        _                       -> case (expandAlias tyCtx tm1, expandAlias tyCtx tm2) of
                                                                                        (Just tm1', Just tm2')          -> Just (TmMrg tm1' tm2')
                                                                                        _                               -> Nothing
expandAlias tyCtx (TmRec name tm)             = TmRec name <$> expandAlias tyCtx tm
expandAlias tyCtx (TmRProj tm name)           = TmRProj <$> expandAlias tyCtx tm <*> Just name
expandAlias tyCtx (TmProj tm i)               = Just $ TmProj tm i
expandAlias tyCtx (TmLam ty tm)               = case expandTyAlias tyCtx ty of
                                                        Just (Source.TAnd t1 t2)     -> TmLam t1 <$> expandAlias tyCtx (TmLam t2 tm)
                                                        Just ty                      -> TmLam ty <$> expandAlias tyCtx tm
                                                        _                            -> Nothing
expandAlias tyCtx (TmFunc name ty tm)         = case expandAlias tyCtx (TmLam ty tm) of
                                                        Just tm'        -> case debruijnTransform name 0 tm' of
                                                                                Just tm''       -> Just $ TmRec name (TmFix tm'')
                                                                                _               -> Nothing 
                                                        _               -> Nothing
expandAlias tyCtx(TmApp tm1 tm2)              = TmApp <$> expandAlias tyCtx tm1 <*> expandAlias tyCtx tm2
expandAlias _ _                               = Nothing

desugar :: Tm -> Maybe Tm
desugar TmCtx                   = Just TmCtx
desugar TmUnit                  = Just TmUnit
desugar (TmLit n)               = Just $ TmLit n
desugar (TmBool b)              = Just $ TmBool b
desugar (TmString s)            = Just $ TmString s
desugar (TmAliasTyp label typ)  = Just TmUnit
desugar (TmBinOp op tm1 tm2)    = case (desugar tm1, desugar tm2) of
                                        (Just tm1', Just tm2') -> Just (TmBinOp op tm1' tm2')
                                        _                       -> Nothing
desugar (TmUnOp op tm)          = TmUnOp op     <$> desugar tm
desugar (TmIf tm1 tm2 tm3)      = TmIf          <$> desugar tm1 <*> desugar tm2 <*> desugar tm3
desugar (TmFix tm)              = TmFix         <$> desugar tm
desugar (TmMrg tm1 tm2)         = TmMrg         <$> desugar tm1 <*> desugar tm2
desugar (TmRec name tm)         = TmRec name    <$> desugar tm
desugar (TmRProj tm name)       = TmRProj       <$> desugar tm <*> Just name
desugar (TmProj tm i)           = case desugar tm of
                                        Just tm' -> Just (TmProj tm' i)
                                        _        -> Nothing
desugar (TmLam ty tm)           = case ty of
                                        Source.TAnd t1 t2               -> desugar (TmLam t1 (TmLam t2 tm))
                                        (Source.TRecord label ty)       -> case debruijnTransform label 0 tm of
                                                                                Just tm'        -> TmLam ty <$> desugar tm'
                                                                                _               -> Nothing
                                        ty                              -> TmLam ty <$> desugar tm
desugar (TmFunc name ty tm)     = case desugar (TmLam ty tm) of
                                        Just tm'        -> case debruijnTransform name 0 tm' of
                                                                Just tm''       -> Just $ TmRec name (TmFix tm'')
                                                                _               -> Nothing 
                                        _               -> Nothing
desugar (TmApp tm1 tm2)         = TmApp <$> desugar tm1 <*> desugar tm2
desugar _                       = Nothing


debruijnTransform :: String -> Int -> Tm -> Maybe Tm
debruijnTransform _ _ TmCtx                     = Just TmCtx
debruijnTransform _ _ TmUnit                    = Just TmUnit
debruijnTransform _ _ (TmLit n)                 = Just $ TmLit n
debruijnTransform _ _ (TmBool b)                = Just $ TmBool b
debruijnTransform _ _ (TmString s)              = Just $ TmString s
debruijnTransform x i (TmBinOp op tm1 tm2)      = case (debruijnTransform x i tm1, debruijnTransform x i tm2) of
                                                        (Just tm1', Just tm2')  -> Just (TmBinOp op tm1' tm2')
                                                        _                       -> Nothing
debruijnTransform x i (TmUnOp op tm)            = case debruijnTransform x i tm of
                                                        (Just tm')      -> Just (TmUnOp op tm')
                                                        _               -> Nothing
debruijnTransform x i (TmIf tm1 tm2 tm3)        = TmIf <$> debruijnTransform x i tm1 <*> debruijnTransform x i tm2 <*> debruijnTransform x i tm3
debruijnTransform x i (TmMrg tm1 tm2)           = TmMrg <$> debruijnTransform x i tm1 <*> debruijnTransform x (i + 1) tm2
debruijnTransform x i (TmRec name tm)           = TmRec name <$> debruijnTransform x i tm
debruijnTransform x i (TmRProj tm name)         = if name == x  then Just $ TmProj tm i
                                                                else Just $ TmRProj tm name
debruijnTransform x i (TmProj tm n)             = TmProj <$> debruijnTransform x i tm <*> Just n
debruijnTransform x i (TmFix tm)                = TmFix <$> debruijnTransform x i tm
debruijnTransform x i (TmLam (Source.TRecord label ty) tm)
                                                = if label == x then Just $ TmLam (Source.TRecord label ty) tm
                                                                else TmLam (Source.TRecord label ty) <$> debruijnTransform x (i + 1) tm
debruijnTransform x i (TmLam ty tm)             = TmLam ty <$> debruijnTransform x (i + 1) tm
debruijnTransform x i (TmApp tm1 tm2)           = TmApp <$> debruijnTransform x i tm1 <*> debruijnTransform x i tm2
debruijnTransform _ _ _                         = Nothing


elaborateBinaryOp :: TmBinOp -> BinaryOp
elaborateBinaryOp (TmArith arithop)
        = case arithop of
                TmAdd   -> Arith Add
                TmSub   -> Arith Sub
                TmMul   -> Arith Mul
                TmDiv   -> Arith Div
                TmMod   -> Arith Mod
elaborateBinaryOp (TmComp compop)
        = case compop of
                TmEql   -> Comp Eql
                TmNeq   -> Comp Neq
                TmLt    -> Comp Lt
                TmLe    -> Comp Le
                TmGt    -> Comp Gt
                TmGe    -> Comp Ge
elaborateBinaryOp (TmLogic logicop)
        = case logicop of
                TmAnd   -> Logic And
                TmOr    -> Logic Or


elaborateTyp :: Source.Typ -> Maybe Core.Typ
elaborateTyp Source.TUnit               = Just Core.TUnit
elaborateTyp Source.TInt                = Just Core.TInt
elaborateTyp Source.TBool               = Just Core.TBool
elaborateTyp Source.TString             = Just Core.TString
elaborateTyp (Source.TAnd ty1 ty2)      = Core.TAnd             <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TArrow ty1 ty2)    = Core.TArrow           <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TRecord label ty)  = Core.TRecord label    <$> elaborateTyp ty
elaborateTyp (Source.TList ty)          = Core.TList            <$> elaborateTyp ty
elaborateTyp (Source.TSum ty1 ty2)      = Core.TSum             <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TPair ty1 ty2)     = Core.TPair            <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TSig tA tB)        = Core.TArrow           <$> elaborateTyp tA  <*> elaborateTyp tB
elaborateTyp (Source.TIden _)           = Nothing

elaborate :: Tm -> Maybe Exp
elaborate TmCtx                         = Just Ctx
elaborate TmUnit                        = Just Unit
elaborate (TmLit n)                     = Just $ Lit n
elaborate (TmBool b)                    = Just $ EBool b
elaborate (TmString s)                  = Just $ EString s
elaborate (TmBinOp op tm1 tm2)          = case (elaborateBinaryOp op, elaborate tm1, elaborate tm2) of
                                                (op', Just e1, Just e2) -> Just (BinOp op' e1 e2)
                                                _                       -> Nothing
elaborate (TmUnOp op tm)                = UnOp <$> Just (surfaceUnaryToCoreOp op) <*> elaborate tm
elaborate (TmIf tm1 tm2 tm3)            = If   <$> elaborate tm1        <*> elaborate tm2 <*> elaborate tm3
elaborate (TmMrg tm1 tm2)               = Mrg  <$> elaborate tm1        <*> elaborate tm2
elaborate (TmRec name tm)               = Rec name    <$> elaborate tm
elaborate (TmProj tm n)                 = Proj <$> elaborate tm <*> Just n               
elaborate (TmRProj tm name)             = RProj       <$> elaborate tm <*> Just name
elaborate (TmLam ty tm)                 = Lam <$> elaborateTyp ty <*> elaborate tm
        -- case ty of
        --                                         (Source.TRecord label ty')        -> Lam <$> elaborateTyp ty' <*> case debruijnTransform label 0 tm of
        --                                                                                                                 Just tm'        -> elaborate tm'
        --                                                                                                                 _               -> Nothing
                                                -- _                                 -> Lam <$> elaborateTyp ty <*> elaborate tm
elaborate (TmApp tm1 tm2)               = App <$> elaborate tm1 <*> elaborate tm2
elaborate (TmFix tm)                    = Fix <$> elaborate tm  
elaborate _                             = Nothing


translate :: Tm -> Maybe Exp
translate tm    = elaborate =<< desugar =<< expandAlias Source.TUnit tm