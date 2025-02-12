{-# LANGUAGE LambdaCase #-}
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
            = if label' == label then Right ty2
                                 else lookupAliasTyp ty1 label
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
-- === Example:
-- >>> expandAlias STUnit (SApp (SLam STInt (SString "Hello")) [SLit 1, SLit 2, SString "Hello"])
-- Right (SApp (SLam STInt (SString "Hello")) [SLit 1,SLit 2,SString "Hello"])
--
-- >>> expandAlias STUnit (SMrg (SAliasTyp "x" STInt) (SLam (STIden "x") (SLit 1)))
-- Right (SLam STInt (SLit 1))
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
expandAlias _ SCtx              = Right SCtx
expandAlias _ SUnit             = Right SUnit
expandAlias _ (SLit i)          = Right (SLit i)
expandAlias _ (SBool b)         = Right (SBool b)
expandAlias _ (SString s)       = Right (SString s)
expandAlias ctx (SLam ty tm)    = SLam <$> expandTyAlias ctx ty <*> expandAlias ctx tm
expandAlias ctx (SClos tm1 ty tm2)
                                = SClos  <$> expandAlias ctx tm1 <*> expandTyAlias ctx ty <*> expandAlias ctx tm2
expandAlias ctx (SRec l tm)     = SRec l <$> expandAlias ctx tm
expandAlias ctx (SRProj tm l)   = SRProj <$> expandAlias ctx tm <*> Right l
expandAlias ctx (SProj tm i)    = SProj  <$> expandAlias ctx tm <*> Right i
expandAlias ctx (SApp tm params)= SApp   <$> expandAlias ctx tm <*> traverse (expandAlias ctx) params
expandAlias ctx (SMrg tm1 tm2)  = case tm1 of
                                        (SAliasTyp l ty) -> expandTyAlias ctx ty >>= (`expandAlias` tm2) . STAnd ctx . STRecord l
                                        _                -> SMrg <$> expandAlias ctx tm1 <*> expandAlias ctx tm2
expandAlias ctx (SBox tm1 tm2)  = SBox  <$> expandAlias ctx tm1 <*> expandAlias ctx tm2
expandAlias _ _                 = Left $ TypeExpansionFailed "Expansion function not completed."
