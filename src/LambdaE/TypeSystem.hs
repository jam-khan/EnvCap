module LambdaE.TypeSystem where
import LambdaE.Syntax (Expr(..))
import LambdaE.Types (Typ (..))


-- Context is a Type
synth :: Typ -> Expr -> Maybe Typ
-- TYP-CTX
synth ctx Ctx                   = Just ctx
-- TYP-LIT
synth ctx (Lit n)               = Just TInt
-- TYP-TOP
synth ctx Unit                  = Just TUnit
-- TYP-APP
synth ctx (BinOp App e1 e2) =
    case synth ctx e1 of
        Just (TArrow t1 t2) -> 
                    if      check ctx e2 t1 
                    then    Just t2 
                    else    Nothing
        _ -> Nothing
-- TYP-MERGE
synth ctx (BinOp Mrg e1 e2) =
    case (synth ctx e1) of
        Just t1 ->
            case synth (TAnd ctx t1) e2 of
            Just t2     -> Just (TAnd t1 t2)
            _           -> Nothing
        _           -> Nothing 
-- TYP-LAM
synth ctx (Lam t1 e) = 
    case synth (TAND ctx t1) e of
        Just t2 -> Just (TArrow t1 t2)
        _       -> Nothing
-- TYP-BOX
synth ctx (BinOp Box e1 e2) =
    case (synth ctx e1) of
        Just ctx1 -> case synth ctx1 e2 of
                            Just tA -> Just tA
                            Nothing -> Nothing
        Nothing   ->    Nothing
-- TYP-CLOS
-- ** IMPLEMENT Bi-directional typing for the below **
synth ctx (Clos expV tA expE) =
    case (synth Unit expV) of
        Just t1     -> case (synth (TAnd t1 tA) expV) of
                            Just tB -> Just (TArrow tA tB)
                            Nothing -> Nothing
        Nothing     -> Nothing

check :: Typ -> Expr -> Typ -> Bool
check ctx Ctx t             = ctx == t
check ctx (Lit n) TInt      = True
check ctx Unit TUnit        = True
check ctx (BinOp App e1 e2) t2 =
    case 
check _ _ _ = False