module LambdaE.BigStep where

import LambdaE.Syntax ( Op(..), Expr(..), Value(..) )
import Data.Maybe (fromMaybe)


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
-- WRITE ** UNIT TESTING **
rlookupv (VMrg v1 v2) label =
    case (rlookupv v1 label, rlookupv v2 label) of
        (Just vL, Nothing)      -> Just vL
        (Nothing, Just vR)      -> Just vR
        (_, _)                  -> Nothing
rlookupv _ _ = Nothing

evalB :: Expr -> Expr -> Maybe Value
evalB e expr = case evalBig VUnit e of
                    Just v -> evalBig v expr
                    _      -> Nothing

-- Big Step Evaluation
-- We assume that eval is gonna get environment as a value
evalBig :: Value -> Expr -> Maybe Value
-- BSTEP-CTX
evalBig env Ctx                   = Just env
-- BSTEP-PROJV
evalBig env (Proj e n)            = lookupv v1 n
                                    where   Just v1 = evalBig env e
-- BSTEP-LIT
evalBig env (Lit n)               = Just (VInt n)
-- BSTEP-CLOS
evalBig env (Clos e1 t e)         = Just (VClos v t e)
                                    where Just v = evalBig env e1
-- BSTEP-UNIT
evalBig env Unit                  = Just VUnit
-- BSTEP-MRG
evalBig env (BinOp Mrg e1 e2)     = Just (VMrg v1 v2)
                                    where   Just v1 = evalBig env e1
                                            Just v2 = evalBig (VMrg env v1) e2
-- BSTEP-BOX
evalBig env (BinOp Box e1 e2)     = evalBig v1 e2
                                    where Just v1 = evalBig env e1
-- BSTEP-APP
evalBig env (BinOp App e1 e2)     = case evalBig env e1 of
                                    Just (VClos v1 t e) -> case evalBig env e2 of
                                                            Just v2     -> evalBig (VMrg v1 v2) e
                                                            _           -> Nothing
                                    _                   -> Nothing
-- BSTEP-LAM
evalBig env (Lam t e)             = Just (VClos env t e)
-- BSTEP-REC
evalBig env (Rec s e)             = Just (VRcd s v)
                                    where Just v = evalBig env e
-- BSTEP-SEL
evalBig env (RProj e s)           = rlookupv v1 s
                                    where   Just v1 = evalBig env e
