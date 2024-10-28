module Core.Semantics where
import Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (TBool))
import Data.Maybe (fromMaybe)
import GHC.GHCi.Helpers (evalWrapper)


lookupv :: Value -> Int -> Maybe Value
lookupv (VMrg v1 v2) 0 = Just v2
lookupv (VMrg v1 v2) n = lookupv v1 (n - 1)
lookupv _ _                 = Nothing


-- record lookup
rlookupv :: Value -> String -> Maybe Value
rlookupv (VRcd l v) label
    | l == label = Just v

-- Avoiding ambigous lookups
-- If lookup label present in both or none
-- then, result is Nothing
-- So, must be present only once
rlookupv (VMrg v1 v2) label =
    case (rlookupv v1 label, rlookupv v2 label) of
        (Just vL, Nothing)      -> Just vL
        (Nothing, Just vR)      -> Just vR
        (_, _)                  -> Nothing
rlookupv _ _ = Nothing

compareWith :: (Ord a) => CompOp -> a -> a -> Bool
compareWith Eql  x y =   x == y
compareWith Neq x y =   x /= y
compareWith Lt  x y =   x < y
compareWith Le  x y =   x <= y
compareWith Gt  x y =   x > y
compareWith Ge  x y =   x >= y

compareOp :: CompOp -> Value -> Value -> Bool
compareOp op  (VInt v1) (VInt v2)     = compareWith op v1 v2
compareOp op  (VBool b1) (VBool b2)   = compareWith op b1 b2

evalB :: Exp -> Exp -> Maybe Value
evalB e exp = case evalBig VUnit e of
                    Just v -> evalBig v exp
                    _      -> Nothing

-- Big Step Evaluation
-- We assume that eval is gonna get environment as a value
evalBig :: Value -> Exp -> Maybe Value
-- BSTEP-CTX
evalBig env Ctx                   = Just env
-- BSTEP-PROJV
evalBig env (Proj e n)            = lookupv v1 n
                                    where   Just v1 = evalBig env e
-- BSTEP-LIT
evalBig env (Lit n)               = Just (VInt n)
-- BSTEP-BOOL
evalBig env (EBool b)             = Just (VBool b)
-- BSTEP-IF
evalBig env (If cond e1 e2)       = case evalBig env cond of
                                    Just (VBool True)   -> evalBig env e1
                                    _                   -> evalBig env e2
-- BSTEP-CLOS
evalBig env (Clos e1 t e)         = Just (VClos v t e)
                                    where Just v = evalBig env e1
-- BSTEP-UNIT
evalBig env Unit                  = Just VUnit
-- BSTEP-BOX
evalBig env (BinOp Box e1 e2)     = evalBig v1 e2
                                    where Just v1 = evalBig env e1
-- BSTEP-APP
evalBig env (BinOp App e1 e2)     = case evalBig env e1 of
                                    Just (VClos v1 t e) -> case evalBig env e2 of
                                                            Just v2     -> evalBig (VMrg v1 v2) e
                                                            _           -> Nothing
                                    _                   -> Nothing
-- BSTEP-MRG
evalBig env (BinOp Mrg e1 e2)     = Just (VMrg v1 v2)
                                    where   Just v1 = evalBig env e1
                                            Just v2 = evalBig (VMrg env v1) e2
-- BSTEP-Arith
evalBig env 
        (BinOp (Arith op) e1 e2) = case op of 
                                        Add     ->  Just (VInt (v1 + v2))
                                        Sub     ->  Just (VInt (v1 - v2))
                                        Mul     ->  Just (VInt (v1 * v2))
                                        Div     ->  if v2 == 0  then Nothing
                                                                else Just (VInt (v1 `div` v2))
                                    where Just (VInt v1)   = evalBig env e1
                                          Just (VInt v2)   = evalBig env e2
-- BSTEP-Comp
evalBig env
        (BinOp (Comp op) e1 e2) =   Just (VBool (compareOp op v1 v2))
                                    where   Just v1 = evalBig env e1
                                            Just v2 = evalBig env e2
-- BSTEP-Logic
evalBig env
        (BinOp (Logic op) e1 e2) =  Just v3
                                    where   Just (VBool b1) = evalBig env e1
                                            Just (VBool b2) = evalBig env e2
                                            v3              = case op of
                                                                And -> VBool (b1 && b2)
                                                                Or  -> VBool (b1 || b2)
-- BSTEP-Not
evalBig env (UnOp Not e1) = evalBig env (BinOp (Logic And) e1 (EBool False))
-- BSTEP-LAM
evalBig env (Lam t e)             = Just (VClos env t e)
-- BSTEP-REC
evalBig env (Rec s e)             = Just (VRcd s v)
                                    where Just v = evalBig env e
-- BSTEP-SEL
evalBig env (RProj e s)           = rlookupv v1 s
                                    where   Just v1 = evalBig env e
