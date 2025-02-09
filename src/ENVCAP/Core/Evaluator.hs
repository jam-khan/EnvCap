{-# LANGUAGE LambdaCase #-}
module ENVCAP.Core.Evaluator where

import ENVCAP.Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), LogicOp(..), Typ(..))
import ENVCAP.Core.Util  (compareOp, lookupv, rlookupv, arithOp)


eval :: Value -> Exp -> Maybe Value
-- BSTEP-LIT
eval _ (Lit n)                  = Just (VInt n)
-- BSTEP-UNIT
eval _ Unit                     = Just VUnit
-- BSTEP-CTX
eval env Ctx                    = Just env
-- BSTEP-VAR/PROJ  
eval env (Proj e n)             = eval env e >>= \v1 -> lookupv v1 n
-- BSTEP-CLOS
eval env (Clos e1 e2)           = eval env e1 >>= \v1 -> return $ VClos v1 e2
-- BSTEP-APP
eval env (App e1 e2)            = eval env e1 >>= \case
                                                (VClos v (Lam _ e))     -> 
                                                        eval env e2 >>= \v2 -> eval (VMrg v v2) e
                                                (VClos v (Fix t (Lam t' e')))   
                                                        -> eval env e2 >>= \v2 ->
                                                                eval (VMrg (VMrg v (VClos v (Fix t (Lam t' e')))) v2) e'
                                                _                       -> Nothing
-- BSTEP-LAM
eval env (Lam t e)              = Just (VClos env (Lam t e))
-- BSTEP-FIX
eval env (Fix ty e)             = Just (VClos env (Fix ty e))
-- BSTEP-REC
eval env (Rec s e)              = eval env e >>= \v -> return $ VRcd s v
-- BSTEP-SEL
eval env (RProj e s)            = eval env e >>= \v1 -> rlookupv v1 s
-- BSTEP-MRG
eval env (Mrg e1 e2)            = eval env e1 >>= \v1 -> eval (VMrg env v1) e2 >>= \v2 -> return $ VMrg v1 v2
-- BSTEP-BOX
eval env (Box e1 e2)            = eval env e1 >>= \v1 -> eval v1 e2
-- BSTEP-Bool
eval _ (EBool b)                = VBool <$> Just b
-- BSTEP-STR
eval _ (EString s)              = VString <$> Just s
-- BSTEP-IF
eval env (If cond e1 e2)        = eval env cond >>= \case
                                                        VBool True   -> eval env e1
                                                        VBool False  -> eval env e2
                                                        _            -> Nothing
-- BSTEP-PAIR
eval env (Pair e1 e2)           = VPair <$> eval env e1 <*> eval env e2
-- BSTEP-FST_PAIR
eval env (Fst e)                = eval env e >>= \case  VPair v1 _ -> return v1
                                                        _          -> Nothing
-- BSTEP-SND_PAIR
eval env (Snd e)                = eval env e >>= \case  VPair _ v2 -> return v2
                                                        _          -> Nothing
-- BSTEP_Inl
eval env (InL t e)              = VInL t <$> eval env e
-- BSTEP_Inr
eval env (InR t e)              = VInR t <$> eval env e
-- BSTEP-Case
eval env (Case e1 e2 e3)        = eval env e1 >>= \case
                                        VInL _ v1 -> eval (VMrg env v1) e2
                                        VInR _ v1 -> eval (VMrg env v1) e3
                                        _         -> Nothing
-- BSTEP-NIL
eval _   (Nil tA)               = Just (VNil tA) 
-- BSTEP-CONS
eval env (Cons e1 e2)           = eval env e1 >>= \v1 -> eval env e2 >>= \v2 -> return $ VCons v1 v2
-- BSTEP-LCASE
eval env (LCase e1 e2 e3)       = eval env e1 >>= \case
                                                        VNil _          -> eval env e2
                                                        VCons v1 v2      -> eval (VMrg (VMrg env v1) v2) e3
                                                        _                -> Nothing
-- BSTEP-ARITH
eval env (BinOp (Arith op) e1 e2)
                                = eval env e1 >>= 
                                        \case 
                                        (VInt v1) -> 
                                                eval env e2 >>= 
                                                        \case
                                                        (VInt v2) -> 
                                                                VInt  <$> arithOp op v1 v2   
                                                        _         -> error "Bad2"
                                        v ->  error ("Bad1: " ++ show env ++ "\n" ++ show v)
-- BSTEP-COMP
eval env (BinOp (Comp op) e1 e2)
                                = eval env e1 >>= 
                                        \v1 -> eval env e2 >>= 
                                                \v2 -> return $ VBool (compareOp op v1 v2)
-- BSTEP-LOGIC
eval env (BinOp (Logic op) e1 e2)
                                = eval env e1 >>= 
                                        \case 
                                        (VBool b1) -> 
                                                eval env e2 >>= 
                                                        \case
                                                        (VBool b2) -> 
                                                                case op of
                                                                And -> return $ VBool (b1 && b2)
                                                                Or  -> return $ VBool (b1 || b2)
                                                        _         -> Nothing
                                        _       -> Nothing
-- BSTEP-NOT
eval env (UnOp Not e1)          = case eval env e1 of
                                        Just (VBool b)       -> Just (VBool (not b))
                                        _                    -> Nothing