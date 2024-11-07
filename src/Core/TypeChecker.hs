module Core.TypeChecker where

import Core.Syntax (Exp(..), Typ(..), Value(..), BinaryOp(..))


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
infer :: Typ -> Exp -> Maybe Typ
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
    case infer ctx e1 of
        Just (TArrow tA tB) ->  if  check ctx e2 tA 
                                then Just tB
                                else Nothing
        _                   -> Nothing
-- TYP-LAM
infer ctx (Lam tA e) = 
    case infer (TAnd ctx tA) e of
        Just tB -> Just (TArrow tA tB)
        _       -> Nothing

-- TYP-CLOS
infer ctx (Clos e1 tA e2) =
    case infer ctx e1 of
        Just ctx1   -> 
            case infer (TAnd ctx1 tA) e2 of
                Just tB     -> Just (TArrow tA tB)
                _           -> Nothing
        _           -> Nothing
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

-- Extensions

{-
                    --------------------------- (TYP-Bool)
                        v |- Bool => TBool

        v |- e1 <= Bool         v |- e2 => t           v |- e3 <= t
        ----------------------------------------------------------- (TYP-IF)
                        v |- If e1 e2 e3 => t1
-}
-- TYP-BOOL
infer ctx (EBool bool)      = Just TBool
-- TYP-IF
infer ctx (If cond e1 e2)   =   if check ctx cond TBool && (t1 == t2)
                                    then Just t1
                                    else Nothing
                                where
                                    Just t1 = infer ctx e1
                                    Just t2 = infer ctx e2




check :: Typ -> Exp -> Typ -> Bool
-- TYP-EQ
check ctx e tA      = case infer ctx e of
                        Just tB -> tA == tB
                        _       -> False


-- First finish above
-- Write unit testing (IMPORTANT)
-- Write property-based testing (IMPORTANT)

