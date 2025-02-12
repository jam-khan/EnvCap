module ENVCAP.Source.LocallyNameless where
import ENVCAP.Source.Errors
import ENVCAP.Syntax (SurfaceTm (SCtx))


debruijnTransform :: String -> Int -> SurfaceTm -> Either LocallyNamelessError SurfaceTm
debruijnTransform _x _i SCtx   = Right SCtx
debruijnTransform _x _i _      = Left $ LocallyNamelessFailed "Function not fully implemented."

astTolocallyNameless :: SurfaceTm -> Either LocallyNamelessError SurfaceTm
astTolocallyNameless SCtx      = Right SCtx
astTolocallyNameless _tm       = Left $ LocallyNamelessFailed "Function not fully implemented."

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
