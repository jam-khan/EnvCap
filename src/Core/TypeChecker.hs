module Core.TypeChecker where

import Core.Syntax (Exp(..), Typ(..), Value(..), BinaryOp(..), UnaryOp(..))
import Core.Semantics (evalBig)
import Test.QuickCheck
import Control.Monad (liftM2, liftM)



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
infer ctx Ctx = Just ctx
-- TYP-PROJ
infer ctx (Proj e n) = do
    tB <- infer ctx e
    lookupt tB n
-- TYP-LIT
infer ctx (Lit i) = Just TInt
-- TYP-TOP
infer ctx Unit = Just TUnit
-- TYP-BOX
infer ctx (Box e1 e2) = do
    ctx1 <- infer ctx e1
    infer ctx1 e2
-- TYP-MERGE
infer ctx (Mrg e1 e2) = do
    tA <- infer ctx e1
    tB <- infer (TAnd ctx tA) e2
    Just (TAnd tA tB)
-- TYP-APP
infer ctx (App e1 e2) = case infer ctx e1 of
    Just (TArrow tA tB) -> if check ctx e2 tA
                           then Just tB
                           else Nothing
    _ -> Nothing
-- TYP-LAM
infer ctx (Lam tA e) = case infer (TAnd ctx tA) e of
    Just tB -> Just (TArrow tA tB)
    _ -> Nothing
-- TYP-CLOS
infer ctx (Clos e1 (Lam tA e2)) = case infer ctx e1 of
    Just ctx1 -> case infer (TAnd ctx1 tA) e2 of
        Just tB -> Just (TArrow tA tB)
        _ -> Nothing
    _ -> Nothing
-- TYP-RCD
infer ctx (Rec l e) = do
    tA <- infer ctx e
    Just (TRecord l tA)
-- TYP-SEL
infer ctx (RProj e l) = do
    tB <- infer ctx e
    tA <- rlookupt tB l
    if containment (TRecord l tA) tB
        then Just tA
        else Nothing
-- TYP-BOOL
infer ctx (EBool _) = Just TBool
-- TYP-STRING
infer ctx (EString _) = Just TString
-- TYP-FIX
infer ctx (Fix (Lam tA e)) = if check (TAnd ctx tA) e (TArrow tA tA)
                             then Just tA
                             else Nothing
-- TYP-LET
infer ctx (Let e1 e2) = do
    tA <- infer ctx e1
    infer (TAnd ctx tA) e2
-- TYP-IF
infer ctx (If cond e1 e2) = do
    if check ctx cond TBool
    then do
        t1 <- infer ctx e1
        t2 <- infer ctx e2
        if t1 == t2
        then Just t1
        else Nothing
    else Nothing
-- TYP-PAIR
infer ctx (Pair e1 e2) = TPair <$> infer ctx e1 <*> infer ctx e2
-- TYP-FST
infer ctx (Fst e) = case infer ctx e of
    Just (TPair tA tB) -> Just tA
    _ -> Nothing
-- TYP-SND
infer ctx (Snd e) = case infer ctx e of
    Just (TPair tA tB) -> Just tB
    _ -> Nothing
-- TYP-INL
infer ctx (InL tB e1) = TSum <$> infer ctx e1 <*> Just tB
-- TYP-INR
infer ctx (InR tA e2) = TSum <$> Just tA <*> infer ctx e2
-- TYP-CASE
infer ctx (Case e1 e2 e3) = case infer ctx e1 of
    Just (TSum t1 t2) -> case infer (TAnd ctx t1) e2 of
        Just t3 -> if check (TAnd ctx t2) e3 t3
                   then Just t3
                   else Nothing
        _ -> Nothing
    _ -> Nothing
-- TYP-NIL
infer ctx (Nil tA) = Just (TList tA)
-- TYP-CONS
infer ctx (Cons t1 t2) = case infer ctx t2 of
    Just (TList tA) -> if check (TAnd ctx tA) t1 tA
                       then Just (TList tA)
                       else Nothing
    _ -> Nothing
-- TYP-LCASE
infer ctx (LCase e1 e2 e3) = case infer ctx e1 of
    Just (TList t1) -> case infer ctx e2 of
        Just (TList t2) -> if check ctx e3 (TAnd (TAnd ctx t2) (TList t2))
                           then Just t2
                           else Nothing
        _ -> Nothing
    _ -> Nothing
-- TYP-ARITH
infer ctx (BinOp (Arith _) e1 e2) = case infer ctx e1 of
    Just TInt -> if check ctx e2 TInt
                 then Just TInt
                 else Nothing
    _ -> Nothing
-- TYP-COMP
infer ctx (BinOp (Comp _) e1 e2) = case infer ctx e1 of
    Just TInt -> if check ctx e2 TInt
                 then Just TBool
                 else Nothing
    Just TBool -> if check ctx e2 TBool
                  then Just TBool
                  else Nothing
    _ -> Nothing
-- TYP-LOGIC
infer ctx (BinOp (Logic _) e1 e2) = case infer ctx e1 of
    Just TBool -> if check ctx e2 TBool
                  then Just TBool
                  else Nothing
    _ -> Nothing
infer ctx (UnOp Not e) = if check ctx e TBool 
                         then Just TBool
                         else Nothing
infer _ _ = Nothing

check :: Typ -> Exp -> Typ -> Bool
check ctx (Nil t1) t2 = t1 == t2
-- TYP-EQ
check ctx e tA = case infer ctx e of
    Just tB -> tA == tB
    _ -> False


-- Write unit testing (IMPORTANT)
-- Write property-based testing (IMPORTANT)
-- Property: Type Preservation
prop_preservation :: Exp -> Property
prop_preservation t = counterexample (show t) $
                        case infer TUnit t of
                            Just ty ->
                                case evalBig VUnit t of
                                    Just v  -> property (getValueTyp v == Just ty)
                                    _       -> property False
                            _   -> property True
main :: IO () 
main = quickCheckWith stdArgs { maxSuccess = 1000 } prop_preservation

getValueTyp :: Value -> Maybe Typ
getValueTyp VUnit       = Just TUnit
getValueTyp (VInt _)    = Just TInt
getValueTyp (VClos v (Lam tA e)) =
    case getValueTyp v of
        Just env    -> (case infer (TAnd TUnit tA) e of
                            Just tB -> Just (TArrow tA tB)
                            _       -> Nothing)
        _       -> Nothing
getValueTyp (VRcd s val) = TRecord s <$> getValueTyp val
getValueTyp (VMrg v1 v2) = TAnd <$> getValueTyp v1 <*> getValueTyp v2
getValueTyp (VBool _)    = Just TBool
getValueTyp (VString _)     = Just TString
getValueTyp (VPair v1 v2)   = TPair <$> getValueTyp v1 <*> getValueTyp v2
getValueTyp (VInL t2 v1)    = TSum <$> getValueTyp v1 <*> Just t2
getValueTyp (VInR t1 v2)    = TSum <$> Just t1 <*> getValueTyp v2
getValueTyp (VNil t)        = Just (TList t)
getValueTyp (VCons v1 v2)   = if (TList <$> getValueTyp v1) == getValueTyp v2
                                then TList <$> getValueTyp v1
                                else Nothing