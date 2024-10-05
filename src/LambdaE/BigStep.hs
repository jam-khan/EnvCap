module LambdaE.BigStep where

import LambdaE.Syntax ( Op(..), Expr(..), Value(..) )
import Data.Maybe (fromMaybe)


data LookupResultV = Found Value | NotFound
    deriving (Eq, Show)

{-
    Coq Definition

    Inductive lookupv : exp -> nat -> exp -> Prop :=
    | lvzero : forall v1 v2, 
        lookupv (binop mrg v1 v2) 0 v2
    | lvsucc : forall v1 v2 n v3, 
        lookupv v1 n v3 -> 
        lookupv (binop mrg v1 v2) (S n) v3.
-}


lookupv :: Value -> Int -> Maybe Value
lookupv (VMrg v1 v2) 0 = Just v2
lookupv (VMrg v1 v2) n = lookupv v1 (n - 1)
lookupv _ _                 = Nothing


-- record lookup
rlookupv :: Value -> String -> Maybe Value
rlookupv (VRcd l e) label
    | l == label = Just e
rlookupv (VMrg v1 v2) label =
    case rlookupv v1 label of
        Just value  -> Just value
        Nothing     -> rlookupv v2 label
rlookupv _ _ = Nothing

-- Example
-- exampleExpr :: Expr
-- exampleExpr = BinOp Mrg (Rec "x" (Lit 1)) (Rec "y" (Lit 100))

-- testRLookupV :: String -> Maybe Expr
-- testRLookupV = rlookupv exampleExpr

-- Evaluate the environment first!
-- Wrapper for Big Step evaluator
evalB :: Expr -> Expr -> Maybe Value
evalB e expr = case evalBig VUnit e of
                    Just v -> evalBig v expr
                    _      -> Nothing

-- Big Step Evaluation
-- We assume that eval is gonna get environment as a value

evalBig :: Value -> Expr -> Maybe Value
evalBig env (Lit 1)               = Just (VInt (Lit 1))
evalBig env Unit                  = Just VUnit
evalBig env Ctx                   = Just env
evalBig env (Clos e1 t e)         = case evalBig env e1 of
                                        Just v  -> Just (VClos v t e)
                                        Nothing -> Nothing
evalBig env 
    (BinOp App (Clos e2 t e) e1)  = evalBig env (BinOp Box (BinOp Mrg e2 e1) e)
evalBig env (BinOp App e1 e2)     = evalBig (VMrg env v1) e2
                                    where Just v1 = evalBig env e1
evalBig env (BinOp Box e1 e2)     = evalBig v1 e2
                                    where Just v1 = evalBig env e1 
evalBig env (BinOp Mrg e1 e2)     = Just (VMrg v1 v2)
                                    where   Just v1 = evalBig env e1
                                            Just v2 = evalBig (VMrg env v1) e2
evalBig env (Lam t e)             = Just (VClos env t e)
evalBig env (Proj e n)            = lookupv v1 n
                                    where   Just v1 = evalBig env e
evalBig env (RProj e s)           = rlookupv v1 s
                                    where   Just v1 = evalBig env e
evalBig _ _                       = Nothing

