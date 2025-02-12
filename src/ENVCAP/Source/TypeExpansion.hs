module ENVCAP.Source.TypeExpansion where
import ENVCAP.Syntax
import ENVCAP.Source.Errors (TypeExpansionError (DuplicateAlias, AliasNotFound, TypeContextError, TypeExpansionFailed))


-- | Finds a `SurfaceTyp` by its alias (`label`) in a nested `STAnd` / `STRecord` type context.
--
-- This function searches for a type alias (`label`) in a type context composed of nested `STAnd`
-- and `STRecord` types. If the alias is found, it returns the corresponding `SurfaceTyp`. If not,
-- it returns an appropriate error.
--
-- === Errors:
-- * `AliasNotFound err`: Occurs when the alias is not found in the context.
-- * `TypeContextError err`: Occurs when the type context is not well-formed.
-- * `DuplicateAlias err`: Occurs when there is a `label` clash, potentially due to multiple types
--   with the same label or alias.
--
-- === Example:
-- >>> lookupAliasTyp (STAnd (STRecord "x" STInt) (STRecord "y" STBool)) "x"
-- Right STBool
--
-- === Notes:
-- * The type context must be well-formed and composed only of `STAnd` and `STRecord` types.
-- * If a duplicate label is found, a `DuplicateAlias` error is returned.
lookupAliasTyp :: SurfaceTyp -> String -> Either TypeExpansionError SurfaceTyp
lookupAliasTyp (STAnd ty1 (STRecord label' ty2)) label 
            = if label' == label    
                then case lookupAliasTyp ty1 label of
                        Right ty     -> 
                            Left $ DuplicateAlias ("Error: Name clash of type alias `" ++ show label ++ "`. Found two types with same label: " ++ show ty ++ ", " ++ show ty2)
                        Left err -> Left err
                else Right ty2
lookupAliasTyp STUnit l = Left $ AliasNotFound ("Error: Type not found for alias: " ++ show l)
lookupAliasTyp _ _      = Left $ TypeContextError "Error: Type context not well-formed. Must only compose of intersection types."


-- | Recursively replaces type aliases (`STIden`) in a `SurfaceTyp` using the type context `tyGamma`.
--
-- This function traverses the type structure and replaces any type alias (`STIden`) with its
-- corresponding definition found in the type context `tyGamma`.
--
-- === Errors:
-- * `AliasNotFound err`: Occurs when the alias is not found in the context.
-- * `TypeContextError err`: Occurs when the type context is not well-formed.
-- * `DuplicateAlias err`: Occurs when there is a `label` clash, potentially due to multiple types
--   with the same label or alias.
--
-- === Example:
-- >>> expandTyAlias (STAnd (STRecord "x" STInt) (STRecord "y" STInt)) (STIden "x")
-- Right STInt
--
-- === Notes:
-- * The function is recursive and handles all possible `SurfaceTyp` constructors.
-- * The type context `tyGamma` is expected to be well-formed; otherwise, a `TypeContextError` may occur.
expandTyAlias :: SurfaceTyp -> SurfaceTyp -> Either TypeExpansionError SurfaceTyp
expandTyAlias _ STUnit                  = Right STUnit
expandTyAlias _ STInt                   = Right STInt
expandTyAlias _ STBool                  = Right STBool
expandTyAlias _ STString                = Right STString
expandTyAlias tyGamma (STAnd ty1 ty2)   = 
        STAnd       <$> expandTyAlias tyGamma ty1 <*> expandTyAlias tyGamma ty2 
expandTyAlias tyGamma (STArrow tyA tyB) =
        STArrow     <$> expandTyAlias tyGamma tyA <*> expandTyAlias tyGamma tyB
expandTyAlias tyGamma (STRecord l ty)   =
        STRecord l  <$> expandTyAlias tyGamma ty
expandTyAlias tyCtx (STList ty)         = 
        STList      <$> expandTyAlias tyCtx ty
expandTyAlias tyCtx (STSum  ty1 ty2)    = 
        STSum           <$> expandTyAlias tyCtx ty1  <*> expandTyAlias tyCtx ty2
expandTyAlias tyCtx (STPair ty1 ty2)    = 
        STPair          <$> expandTyAlias tyCtx ty1  <*> expandTyAlias tyCtx ty2
expandTyAlias tyCtx (STSig  tyA tyB)    = 
        STSig           <$> expandTyAlias tyCtx tyA  <*> expandTyAlias tyCtx tyB
expandTyAlias tyCtx (STIden label)      = 
        lookupAliasTyp tyCtx label

-- | Recursively traverses the surface level AST and expands type aliases.
--
-- This function traverses the surface level AST and loads the type aliases in the context
-- and then, expands any type identifier by calling expandTyAlias.
--
-- === Errors:
-- * `AliasNotFound err`: Occurs when the alias is not found in the context.
-- * `TypeContextError err`: Occurs when the type context is not well-formed.
-- * `DuplicateAlias err`: Occurs when there is a `label` clash, potentially due to multiple types
--   with the same label or alias.
--
-- === Notes:
-- * The function is recursive and handles all possible `SurfaceTyp` constructors.
-- * The type context `tyGamma` is expected to be well-formed; otherwise, a `TypeContextError` may occur.
expandAlias :: SurfaceTyp -> SurfaceTm -> Either TypeExpansionError SurfaceTm
expandAlias _ SCtx  = Right SCtx
expandAlias _ _     = Left $ TypeExpansionFailed "Expansion function not completed."

-- expandAlias :: SurfaceTyp -> SurfaceTm -> Maybe SurfaceTm 
-- expandAlias _     SCtx                       = Just SCtx
-- expandAlias _     SUnit                      = Just SUnit
-- expandAlias _     (SLit n)                   = Just $ SLit n
-- expandAlias _     (SBool b)                  = Just $ SBool b
-- expandAlias _     (SString s)                = Just $ SString s
-- expandAlias _     (SAliasTyp _ _)            = error "Type aliases expansion not completed properly"
-- expandAlias tyCtx (SBinOp op tm1 tm2)        = 
--                 case (expandAlias tyCtx tm1, expandAlias tyCtx tm2) of
--                         (Just tm1', Just tm2') -> Just (SBinOp op tm1' tm2')
--                         _                      -> Nothing
-- expandAlias tyCtx (SUnOp op tm)              = 
--         SUnOp op       <$> expandAlias tyCtx tm
-- expandAlias tyCtx (SIf tm1 tm2 tm3)          = 
--         SIf            <$> expandAlias tyCtx tm1 <*> expandAlias tyCtx tm2 <*> expandAlias tyCtx tm3
-- expandAlias tyCtx (SFix tm)                  = 
--         SFix           <$> expandAlias tyCtx tm
-- expandAlias tyCtx (SMrg tm1 tm2)             = 
--                 case tm1 of 
--                 (SAliasTyp label typ)  -> expandAlias (STAnd tyCtx (STRecord label typ)) tm2
--                 _                       -> case (expandAlias tyCtx tm1, expandAlias tyCtx tm2) of
--                                                 (Just tm1', Just tm2')          -> Just (SMrg tm1' tm2')
--                                                 _                               -> Nothing
-- expandAlias tyCtx (SRec name tm)             = 
--         SRec name <$> expandAlias tyCtx tm
-- expandAlias tyCtx (SRProj tm name)           = 
--         SRProj <$> expandAlias tyCtx tm <*> Just name
-- expandAlias _ (SProj tm i)                   = 
--         Just $ SProj tm i
-- expandAlias tyCtx (SLam ty tm)               = 
--         SLam <$> expandTyAlias tyCtx ty <*> expandAlias tyCtx tm
-- expandAlias tyCtx (SFunc name ty tm)         = 
--         SFunc name <$> expandTyAlias tyCtx ty <*> expandAlias tyCtx tm
-- expandAlias tyCtx(SApp tm1 tm2)              = 
--         SApp <$> expandAlias tyCtx tm1 <*> expandAlias tyCtx tm2
-- expandAlias _ _                               = Nothing
