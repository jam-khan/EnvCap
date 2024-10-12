module LambdaE.TypeSystem where
import LambdaE.Syntax (Expr(..), Typ(..), Value(..), Op(..))


-- Lookup based on indexing
lookupt :: Typ -> Int -> Maybe Typ
lookupt (TAnd tA tB) 0   = Just tB
lookupt (TAnd tA tB) n   = lookupt tA (n - 1)
lookupt _ _              = Nothing

-- checks if l is a label in the typing context
isLabel :: String -> Typ -> Bool
isLabel l (TRecord label _)     = l == label
isLabel l (TAnd tA tB)          = isLabel l tA || isLabel l tB
isLabel _ _                     = False

-- containment
-- checks if no duplicate labels exist
containment :: Typ -> Typ -> Bool
containment (TRecord l tA) (TRecord label typ ) =
    l == label && tA == typ
containment (TRecord l tA) (TAnd tB tC)         =
    (containment (TRecord l tA) tB && not (isLabel l tC)) ||
    (containment (TRecord l tA) tC && not (isLabel l tB))
containment _ _                                 = False

-- Lookup based on label
rlookupt :: Typ -> String -> Maybe Typ
rlookupt (TRecord l t) label
    | l == label = Just t
rlookupt (TAnd tA tB) label =
    case rlookupt tB label of
        Just t    -> Just t
        Nothing   -> rlookupt tA label
rlookupt _ _                = Nothing

-- ==> direction
infer :: Typ -> Expr -> Maybe Typ
-- TYP-CTX
infer ctx Ctx               = Just ctx
-- TYP-PROJ
infer ctx (Proj e n)        = lookupt tB n
                            where Just tB = infer ctx e
-- TYP-LIT
infer ctx (Lit i)           = Just TInt
-- TYP-TOP
infer ctx Unit              = Just TUnit
-- TYP-BOX
infer ctx (BinOp Box e1 e2) = infer ctx1 e2
                            where Just ctx1 = infer ctx e1
-- TYP-MERGE
infer ctx (BinOp Mrg e1 e2) = Just (TAnd tA tB)
                            where   Just tA = infer ctx e1
                                    Just tB = infer (TAnd ctx tA) e2
-- TYP-APP
infer ctx (BinOp App e1 e2) =
        if check ctx e2 tA  then Just tB
                            else Nothing
        where Just (TArrow tA tB) = infer ctx e1
-- TYP-LAM  ** Check **
-- TYP-CLOS ** Check **
-- TYP-RCD
infer ctx (Rec l e)     = Just (TRecord l tA)
                        where Just tA = infer ctx e
-- TYP-SEL
infer ctx (RProj e l)   =
    case infer ctx e of
        Just tB     -> case rlookupt tB l of
                        Just tA     -> if containment (TRecord l tA) tB
                                            then Just tA else Nothing
                        Nothing     -> Nothing
        Nothing     -> Nothing
infer _ _ = Nothing

check :: Typ -> Expr -> Typ -> Bool
-- TYP-LAM
check ctx (Lam t e) (TArrow tA tB) = 
    t == tA && check (TAnd ctx tA) e tB
-- TYP-CLOS
check ctx (Clos e1 t e2) (TArrow tA tB) =
    case infer ctx e1 of
        Just ctx1       -> check (TAnd ctx1 tA) e2 tB
        Nothing         -> False
-- TYP-EQ
check ctx e tA      = case infer ctx e of
                        Just tB -> tA == tB
                        _       -> False

-- First finish above
-- Write unit testing (IMPORTANT)
-- Write property-based testing (IMPORTANT)


-- Extensions
    -- Booleans
    -- Conditionals
    -- Arithmetic
    -- Recursion
    -- Let bindings

