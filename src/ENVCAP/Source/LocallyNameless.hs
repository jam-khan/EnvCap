module ENVCAP.Source.LocallyNameless where
import ENVCAP.Source.Errors
import ENVCAP.Syntax

type Depth = Integer
-- | `debruijnIndex stack var i` finds the De Bruijn index of `var` in `stack` starting at index `i`.
--   Returns `Just index` or `Nothing` if not found.
--
-- === Example:
-- >>> debruijnIndex [("x", STInt), ("y", STBool)] "x"
-- Just 1
debruijnIndex :: Params -> String -> Maybe Integer
debruijnIndex stack l = findDebruijnIndex (reverse stack) l 0 

-- | `findDebruijnIndex stack var l i` finds the De Bruijn in the stack by matching the first identifier found.
--  Returns `Just index` or `Nothing` if not found.
--
-- === Example:
-- >>> findDebruijnIndex [("x", STInt)] "x" 0
-- Just 0
-- >>> findDebruijnIndex [ ] "x" 0
-- Nothing
findDebruijnIndex :: Params -> String -> Depth -> Maybe Integer
findDebruijnIndex [] _ _            = Nothing
findDebruijnIndex ((x, _):xs) l i   = 
    if l == x then Just i else findDebruijnIndex xs l (i + 1)

-- | Converts an AST to a locally nameless representation.
-- 
-- This function replaces variable names with De Bruijn indices.
-- 
-- Returns:
-- `Right SurfaceTm`: The AST with variables removed.
-- `Left LocallyNamelessError`: An error if the conversion fails.
--
-- === Example:
-- >>> astToLocallyNameless [("x", STInt)] (SVar "x")
-- Right (SProj SCtx 0)
--
-- >>> astToLocallyNameless [("x", STInt)] (SVar "y")
-- Right (SRProj SCtx "y")
--
-- >>> astToLocallyNameless [] (SLam [("x", STInt), ("y", STInt)] (SBinOp (Arith Add) (SVar "x") (SVar "y")))
-- Right (SLam [("x",STInt),("y",STInt)] (SBinOp (Arith Add) (SProj SCtx 1) (SProj SCtx 0)))
astToLocallyNameless :: Params -> SurfaceTm -> Either LocallyNamelessError SurfaceTm
astToLocallyNameless _ SCtx                 = Right SCtx
astToLocallyNameless _ SUnit                = Right SUnit
astToLocallyNameless _ (SLit i)             = Right (SLit i)
astToLocallyNameless _ (SBool b)            = Right (SBool b)
astToLocallyNameless _ (SString s)          = Right (SString s)
astToLocallyNameless stack (SLam params tm)   =
    SLam <$> Right params <*> astToLocallyNameless (stack ++ params) tm
astToLocallyNameless stack (SClos tm1 params tm2)
    = SClos <$> astToLocallyNameless params tm1 
            <*> Right params 
            <*> astToLocallyNameless (stack ++ params) tm2
astToLocallyNameless stack (SRec l tm)      = 
    SRec l    <$> astToLocallyNameless stack tm
astToLocallyNameless stack (SRProj tm l)    = 
    SRProj    <$> astToLocallyNameless stack tm   <*> Right l
astToLocallyNameless stack (SApp tm1 terms) = 
    SApp      <$> astToLocallyNameless stack tm1  <*> processArguments stack terms
astToLocallyNameless stack (SMrg tm1 tm2)   = 
    SMrg        <$> astToLocallyNameless stack tm1  
                <*> astToLocallyNameless (stack ++ [("?", STUnit)]) tm2
astToLocallyNameless stack (SBox tm1 tm2)   =
    SBox      <$> astToLocallyNameless stack tm1  <*> astToLocallyNameless stack tm2
astToLocallyNameless stack (SVar var)       = 
    case debruijnIndex stack var of
        Just i          -> Right (SProj SCtx i)
        Nothing         -> Right (SRProj SCtx var)
astToLocallyNameless _ (SStruct params tm)    = 
    SStruct params <$> astToLocallyNameless params tm   -- Modules are encapsulated and hence, scope is empty except arguments
astToLocallyNameless stack (SFunc name params typ tm) 
                                                = SFunc name params typ 
                                                    <$> astToLocallyNameless (stack ++ params) tm
astToLocallyNameless _ (SModule name params tm) 
                                                = SModule name params <$> astToLocallyNameless params tm
astToLocallyNameless stack (SLet params tm)   = 
    SLet params     <$> astToLocallyNameless (stack ++ processLetArguments params) tm
astToLocallyNameless stack (SLetrec params tm)= 
    SLetrec params  <$> astToLocallyNameless (stack ++ processLetArguments params) tm
astToLocallyNameless stack (SBinOp op tm1 tm2)  =
    SBinOp op <$> astToLocallyNameless stack tm1 <*> astToLocallyNameless stack tm2
astToLocallyNameless stack (SUnOp op tm)    = 
    SUnOp op  <$> astToLocallyNameless stack tm
astToLocallyNameless _ (SAliasTyp l ty)     = 
    Left $ LocallyNamelessFailed ("Type alias not resolved: type " ++ l ++ " = " ++ show ty)
astToLocallyNameless _stack _tm             = 
    Left $ LocallyNamelessFailed "Function not fully implemented."

-- | Helper for `astToLocallyNameless`
processArguments :: Params -> [SurfaceTm] -> Either LocallyNamelessError [SurfaceTm]
processArguments _params []       = Right []
processArguments params (x:xs)    = astToLocallyNameless params x >>= \x' ->
                                    processArguments params xs >>= \xs' -> Right (x':xs')
-- | Helper for `astToLocallyNameless`
processLetArguments :: [(String, SurfaceTyp, SurfaceTm)] -> [(String, SurfaceTyp)]
processLetArguments []              = []
processLetArguments ((x, ty, _):xs) = (x, ty) : processLetArguments xs

{--
    Suppose programmer wants to create a closure
    So, what can we do?

    "Test1.eps"
    "Test1.epi"
    "Test2.epn"
    "Test2.epi"
    ---------
    secure module Test1
        imports Nothing

        val x = 1;;
        val newEnv = {"x" = 10};;
        
        closure<newEnv> addOne(y:Int) { x + y };;

        function addTwo(y:Int) { x + y };;

        val yOne = box(newEnv, x + 1);; ~~> 11
        val yTwo = x + 1;;              ~~> 2 

        val resultOne = addOne(1);; ~~~> 11
        val resultTwo = addTwo(1);; ~~~> 2
    
    run Test;;

    ~~~>
    {"Test" = 
        struct unit {
            {"x"        = 1},,
            {"newEnv"   = {"x" = 10}},,
            {"addOne"   = <?.newEnv, λInt. ?.x + ?.0>},,
            {"addTwo"   = <?, λInt. ?.x + ?.0>},,
            {"yOne"     = ?.newEnv ▶ ?.x + 1},,
            {"yTwo"     = ?.x + 1},,
            {"resultOne"= (?.addOne 1)},,
            {"resultTwo"= (?.addTwo 1)}
        }
    },,
    ?.Test unit ~~~> this will run it

--}
