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

lookupv :: Expr -> Int -> Maybe Expr
lookupv (BinOp Mrg e1 e2) 0 = Just e2
lookupv (BinOp Mrg e1 e2) n = lookupv e1 (n - 1)
lookupv _ _                 = Nothing


-- record lookup
rlookupv :: Expr -> String -> Maybe Expr
rlookupv (Rec l e) label
    | l == label = Just e
rlookupv (BinOp Mrg e1 e2) label =
    case rlookupv e1 label of
        Just value  -> Just value
        Nothing     -> rlookupv e2 label
rlookupv _ _ = Nothing

-- Example
exampleExpr :: Expr
exampleExpr = BinOp Mrg (Rec "x" (Lit 1)) (Rec "y" (Lit 100))

testRLookupV :: String -> Maybe Expr
testRLookupV = rlookupv exampleExpr



-- Big Step Evaluation
evalBig :: Expr -> Expr -> Expr
evalBig Γ (Lit 1)               = (Lit 1)
evalBig Γ Unit                  = Unit
evalBig Γ Ctx                   = Γ
evalBig Γ (BinOp Mrg e1 e2)     = BinOp Mrg v1 v2
                                    where   v1 = evalBig Γ e1
                                            v2 = evalBig (BinOp Mrg Γ v1) e2
evalBig Γ (BinOp Mrg e1 e2)     = BinOp 