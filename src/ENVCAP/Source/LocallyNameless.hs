module ENVCAP.Source.LocallyNameless where
import ENVCAP.Source.Errors
import ENVCAP.Syntax


debruijnTransform :: String -> Int -> SurfaceTm -> Either LocallyNamelessError SurfaceTm
debruijnTransform _x _i SCtx   = Right SCtx
debruijnTransform _x _i _      = Left $ LocallyNamelessFailed "Function not fully implemented."

astToLocallyNameless :: Args -> SurfaceTm -> Either LocallyNamelessError SurfaceTm
astToLocallyNameless _ SCtx               = Right SCtx
astToLocallyNameless _ SUnit              = Right SUnit
astToLocallyNameless _ (SLit i)           = Right (SLit i)
astToLocallyNameless _ (SBool b)          = Right (SBool b)
astToLocallyNameless _ (SString s)        = Right (SString s)
astToLocallyNameless stack (SLam args tm) = astToLocallyNameless (stack ++ args) tm
astToLocallyNameless _stack _tm           = Left $ LocallyNamelessFailed "Function not fully implemented."


-- expandAlias _ SCtx              = Right SCtx
-- expandAlias _ SUnit             = Right SUnit
-- expandAlias _ (SLit i)          = Right (SLit i)
-- expandAlias _ (SBool b)         = Right (SBool b)
-- expandAlias _ (SString s)       = Right (SString s)
-- expandAlias ctx (SLam ty tm)    = SLam <$> expandTyAlias ctx ty <*> expandAlias ctx tm
-- expandAlias ctx (SClos tm1 ty tm2)
--                                 = SClos  <$> expandAlias ctx tm1 <*> expandTyAlias ctx ty <*> expandAlias ctx tm2
-- expandAlias ctx (SRec l tm)     = SRec l <$> expandAlias ctx tm
-- expandAlias ctx (SRProj tm l)   = SRProj <$> expandAlias ctx tm <*> Right l
-- expandAlias ctx (SProj tm i)    = SProj  <$> expandAlias ctx tm <*> Right i
-- expandAlias ctx (SApp tm params)= SApp   <$> expandAlias ctx tm <*> traverse (expandAlias ctx) params
-- expandAlias ctx (SMrg tm1 tm2)  = case tm1 of
--                                         (SAliasTyp l ty) -> expandTyAlias ctx ty >>= (`expandAlias` tm2) . STAnd ctx . STRecord l
--                                         _                -> SMrg <$> expandAlias ctx tm1 <*> expandAlias ctx tm2
-- expandAlias ctx (SBox tm1 tm2)  = SBox  <$> expandAlias ctx tm1 <*> expandAlias ctx tm2

-- debruijnTransform _ _ SUnit                    = Just SUnit
-- debruijnTransform _ _ (SLit n)                 = Just $ SLit n
-- debruijnTransform _ _ (SBool b)                = Just $ SBool b
-- debruijnTransform _ _ (SString s)              = Just $ SString s
-- debruijnTransform x i (SBinOp op tm1 tm2)      = case (debruijnTransform x i tm1, debruijnTransform x i tm2) of
--                                                         (Just tm1', Just tm2')  -> Just (SBinOp op tm1' tm2')
--                                                         _                       -> Nothing
-- debruijnTransform x i (SUnOp op tm)            = case debruijnTransform x i tm of
--                                                         (Just tm')      -> Just (SUnOp op tm')
--                                                         _               -> Nothing
-- debruijnTransform x i (SIf tm1 tm2 tm3)        = SIf          <$> debruijnTransform x i tm1   <*> debruijnTransform x i tm2   <*> debruijnTransform x i tm3
-- debruijnTransform x i (SMrg tm1 tm2)           = SMrg         <$> debruijnTransform x i tm1   <*> debruijnTransform x (i + 1) tm2
-- debruijnTransform x i (SRec name tm)           = SRec name    <$> debruijnTransform x i tm
-- debruijnTransform x i (SRProj SCtx l)          = Just $ if l == x then SProj SCtx i else SRProj SCtx l
-- debruijnTransform x i (SRProj tm name)         = SRProj       <$> debruijnTransform x i tm    <*> Just name
-- debruijnTransform x i (SProj tm n)             = SProj        <$> debruijnTransform x i tm    <*> Just n
-- debruijnTransform x i (SFix tm)                = SFix         <$> debruijnTransform x i tm
-- debruijnTransform x i (SLam ty tm)             =
--         case ty of
--                 (STRecord label _) ->     if label == x   then Just $ SLam ty tm
--                                                                         else SLam ty    <$> debruijnTransform x (i + 1) tm
--                 _       -> SLam ty      <$> debruijnTransform x (i + 1) tm
-- debruijnTransform x i (SApp tm1 tm2)           = SApp <$> debruijnTransform x i tm1 <*> debruijnTransform x i tm2
-- debruijnTransform _ _ _                         = Nothing
