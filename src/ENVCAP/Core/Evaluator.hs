module ENVCAP.Core.Evaluator where

import ENVCAP.Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import Data.Maybe (fromMaybe)
import ENVCAP.Core.Util  (apply, proj, sub, mult, add, compareOp, lookupv, rlookupv, arithOp)


eval :: Value -> Exp -> Maybe Value
-- BSTEP-CTX
eval env Ctx                    = Just env
-- BSTEP-LIT
eval env (Lit n)                = Just (VInt n)
-- BSTEP-UNIT
eval env Unit                   = Just VUnit
-- BSTEP-VAR/PROJ  
eval env (Proj e n)             = lookupv v1 n
                                    where   Just v1 = eval env e
-- BSTEP-CLOS
eval env (Clos e1 e2)           = Just (VClos v1 e2)
                                    where Just v1 = eval env e1
-- BSTEP-APP
eval env (App e1 e2)            = case eval env e1 of
                                        Just (VClos v1 (Lam t e)) -> case eval env e2 of
                                                                Just v2     -> eval (VMrg v1 v2) e
                                                                _           -> Nothing
                                        Just (VClos v1 (Fix (Lam tA e)))
                                                                -> case eval env e2 of
                                                                        Just v2 -> eval (VMrg (VMrg env (VClos v1 (Fix (Lam tA e)))) v2) e
                                                                        _       -> Nothing
                                        _                   -> Nothing
-- BSTEP-LAM
eval env (Lam t e)              = Just (VClos env (Lam t e))
-- BSTEP-REC
eval env (Rec s e)              = Just (VRcd s v)
                                        where Just v = eval env e
-- BSTEP-SEL
eval env (RProj e s)            = rlookupv v1 s
                                where   Just v1 = eval env e
-- BSTEP-MRG
eval env (Mrg e1 e2)            = Just (VMrg v1 v2)
                                    where   Just v1 = eval env e1
                                            Just v2 = eval (VMrg env v1) e2
-- BSTEP-BOX
eval env (Box e1 e2)            = eval v1 e2
                                    where Just v1 = eval env e1

-- EXTENSIONS
-- BSTEP-True
eval env (EBool True)           = VBool <$> Just True
-- BSTEP-False
eval env (EBool False)          = VBool <$> Just True
-- BSTEP-STR
eval env (EString s)            = VString <$> Just s
-- BSTEP-FIX
eval env (Fix e)                = case eval env e of
                                        Just (VClos v1 (Lam tA e1)) -> Just (VClos v1 (Fix (Lam tA e1)))
                                        _                           -> Nothing

-- BSTEP-IF
eval env (If cond e1 e2)        = case eval env cond of
                                    Just (VBool True)   -> eval env e1
                                    _                   -> eval env e2
-- BSTEP-PAIR
eval env (Pair e1 e2)           = VPair <$> eval env e1 <*> eval env e2

-- BSTEP-FST_PAIR
eval env (Fst e)                = case eval env e of
                                        Just (VPair v1 v2) -> Just v1
                                        _                  -> Nothing
-- BSTEP-SND_PAIR
eval env (Snd e)                = case eval env e of
                                        Just (VPair v1 v2) -> Just v2
                                        _                  -> Nothing
-- BSTEP_Inl
eval env (InL t e)              = VInL t <$> eval env e
-- BSTEP_Inr
eval env (InR t e)              = VInR t <$> eval env e
-- BSTEP-Case
eval env (Case e1 e2 e3)        = case eval env e1 of
                                        Just (VInL t v1) -> eval (VMrg env v1) e2
                                        Just (VInR t v1) -> eval (VMrg env v1) e3
                                        _                -> Nothing
-- BSTEP-NIL
eval env (Nil tA)               = Just (VNil tA)
-- BSTEP-CONS
eval env (Cons e1 e2)           = case eval env e1 of
                                        Just v1         -> case eval (VMrg env v1) e2 of
                                                                Just v2         -> Just (VCons v1 v2)
                                                                _               -> Nothing
                                        _               -> Nothing
-- BSTEP-LCASE
eval env (LCase e1 e2 e3)       = case eval env e1 of
                                        Just (VNil typ)         -> eval env e2
                                        Just (VCons v1 v2)      -> eval (VMrg (VMrg env v1) v2) e3
                                        _                       -> Nothing
-- BSTEP-ARITH
eval env (BinOp (Arith op) e1 e2) 
                                = VInt <$> arithOp op v1 v2
                                        where   Just (VInt v1)   = eval env e1
                                                Just (VInt v2)   = eval env e2
-- BSTEP-COMP
eval env (BinOp (Comp op) e1 e2) 
                                =   Just (VBool (compareOp op v1 v2))
                                        where   Just v1 = eval env e1
                                                Just v2 = eval env e2
-- BSTEP-LOGIC
eval env (BinOp (Logic op) e1 e2)
                                =  Just v3
                                    where   Just (VBool b1) = eval env e1
                                            Just (VBool b2) = eval env e2
                                            v3              = case op of
                                                                And -> VBool (b1 && b2)
                                                                Or  -> VBool (b1 || b2)
-- BSTEP-NOT
eval env (UnOp Not e1)          = case eval env e1 of
                                        Just (VBool b)       -> Just (VBool (not b))
                                        _                    -> Nothing
-- BSTEP-LET    
eval env (Let e1 e2)            = eval (VMrg env v1) e2
                                        where Just v1 = eval env e1