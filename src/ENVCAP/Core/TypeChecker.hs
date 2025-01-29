{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module ENVCAP.Core.TypeChecker where

import ENVCAP.Core.Syntax (Exp(..), Typ(..), Value(..), BinaryOp(..), UnaryOp(..))
import ENVCAP.Core.Util (rlookupt, lookupt, containment)
import ENVCAP.Core.Evaluator (eval)
import Test.QuickCheck
import Control.Monad (liftM2, liftM)
import Data.Maybe (isNothing)

-- Define a custom error type for type inference
data TypeError = TypeError String deriving Show

-- Update the infer function to return Either TypeError Typ
infer :: Typ -> Exp -> Either TypeError Typ
-- TYP-CTX
infer ctx Ctx                   = Right ctx
-- TYP-PROJ
infer ctx (Proj e n)            = do
                                    tB <- infer ctx e
                                    case lookupt tB n of
                                        Just t -> Right t
                                        Nothing -> Left $ TypeError $ "Projection " ++ 
                                                                            show n      ++ " failed on type " 
                                                                            ++ show tB  ++ " ctx: " 
                                                                            ++ show ctx
-- TYP-LIT
infer ctx (Lit i)               = Right TInt
-- TYP-TOP
infer ctx Unit                  = Right TUnit
-- TYP-BOX
infer ctx (Box e1 e2)           = do
                                    ctx1 <- infer ctx e1
                                    infer ctx1 e2
-- TYP-MERGE
infer ctx (Mrg e1 e2)           = do
                                    tA <- infer ctx e1
                                    tB <- infer (TAnd ctx tA) e2
                                    Right (TAnd tA tB)
-- TYP-APP
infer ctx (App e1 e2)           = do
                                    tE1 <- infer ctx e1
                                    case tE1 of
                                        TArrow tA tB -> if check ctx e2 tA then Right tB else Left $ TypeError ("Type mismatch in application: Context:" ++ show ctx)
                                        _            -> Left $ TypeError ("Expected a function type in application \t Function Type: " ++ show tE1 ++ "\t Function: " ++ show e1)
-- TYP-LAM
infer ctx (Lam tA e)            = do
                                    tB <- infer (TAnd ctx tA) e
                                    Right (TArrow tA tB)
-- TYP-CLOS
infer ctx (Clos e1 (Lam tA e2)) = do
                                    ctx1 <- infer ctx e1
                                    tB <- infer (TAnd ctx1 tA) e2
                                    Right (TArrow tA tB)
-- TYP-RCD
infer ctx (Rec l e)             = do
                                    tA <- infer ctx e
                                    Right (TRecord l tA)
-- TYP-SEL
infer ctx (RProj e l)           = do
                                    tB <- infer ctx e
                                    case rlookupt tB l of 
                                        Just tA -> if containment (TRecord l tA) tB 
                                                then Right tA 
                                                else Left $ TypeError "Record projection failed due to containment"
                                        Nothing -> Left $ TypeError $ "Field " ++ show l ++ " not found in type " ++ show tB 
-- TYP-BOOL
infer ctx (EBool _)             = Right TBool
-- TYP-STRING
infer ctx (EString _)           = Right TString 
-- TYP-FIX 
infer ctx (Fix (Lam tA e))      = infer (TAnd ctx (TArrow tA tA)) (Lam tA e)
-- TYP-LET 
infer ctx (Let e1 e2)           = do 
                                    tA <- infer ctx e1 
                                    infer (TAnd ctx tA) e2 
-- TYP-IF 
infer ctx (If cond e1 e2)       = do 
                                    if check ctx cond TBool 
                                        then 
                                            do 
                                                t1 <- infer ctx e1 
                                                t2 <- infer ctx e2 
                                                if t1 == t2 
                                                    then Right t1 
                                                    else Left $ TypeError "Branches of if must have the same type"
                                        else 
                                            Left $ TypeError "Condition must be of type Bool"
-- TYP-PAIR 
infer ctx (Pair e1 e2)          = do 
                                    ta <- infer ctx e1 
                                    tb <- infer ctx e2 
                                    Right $ TPair ta tb 
-- TYP-FST 
infer ctx (Fst e)               = do 
                                    te <- infer ctx e 
                                    case te of 
                                        TPair ta tb -> Right ta 
                                        _           -> Left $ TypeError "Expected a pair type"
-- TYP-SND 
infer ctx (Snd e)               = do 
                                    te <- infer ctx e 
                                    case te of 
                                        TPair ta tb -> Right tb 
                                        _           -> Left $ TypeError "Expected a pair type"
-- TYP-INL 
infer ctx (InL tb e1)           = do 
                                    ta <- infer ctx e1 
                                    Right $ TSum ta tb  
-- TYP-INR 
infer ctx (InR ta e2)           = do  
                                    tb <- infer ctx e2  
                                    Right $ TSum ta tb  
-- TYP-CASE 
infer ctx (Case e1 e2 e3)       = do  
                                    te1 <- infer ctx e1  
                                    case te1 of  
                                        TSum ta tb -> do  
                                            te2 <- infer (TAnd ctx ta) e2  
                                            if check (TAnd ctx tb) e3 te2 then Right te2 else Left $ TypeError "Case branches must match"
                                        _ -> Left $ TypeError "Expected a sum type in case expression"
-- TYP-NIL 
infer ctx (Nil typ)             = Right (TList typ)
-- TYP-CONS 
infer ctx (Cons te te2)         = case infer ctx te2 of
                                    Right (TList tA)            -> if check ctx te tA 
                                                                        then Right (TList tA)
                                                                        else Left $ TypeError "Type mismatch in list constructor"
                                    _                           -> Left $ TypeError "Expected a list type or Null type"
-- TYP-LCASE 
infer ctx (LCase e1 e2 e3)      = do  
                                    te1 <- infer ctx e1  
                                    case te1 of  
                                        TList ta -> do  
                                            te2 <- infer ctx e2  
                                            case te2 of  
                                                TList tb -> if check ctx e3 tb then Right tb else Left $ TypeError "Type mismatch in list case branches"
                                                _ -> Left $ TypeError "Expected a list type for second branch"  
                                        _ -> Left $ TypeError "Expected a list type for first branch"  
-- TYP-ARITH 
infer ctx (BinOp (Arith _) e1 e2) 
                                = do
                                    ti <- infer ctx e1
                                    t2 <- infer ctx e2
                                    if ti == TInt && check ctx e2 TInt  
                                        then Right TInt 
                                        else Left $ TypeError ("Type mismatch in arithmetic operation " ++ show ti ++ " " ++ show t2)
-- TYP-COMP   
infer ctx (BinOp (Comp _) e1 e2) 
                                = do   
                                    ti <- infer ctx e1   
                                    case ti of   
                                        TInt    ->  if check ctx e2 TInt 
                                                        then Right TBool 
                                                        else Left $ TypeError "Type mismatch in comparison operation"   
                                        TBool   ->  if check ctx e2 TBool 
                                                        then Right TBool 
                                                        else Left $ TypeError "Type mismatch in comparison operation"   
                                        _       ->  Left $ TypeError "Expected an integer or boolean for comparison"   
-- TYP-LOGIC   
infer ctx (BinOp (Logic _) e1 e2) 
                                = do   
                                    ti <- infer ctx e1   
                                    if ti == TBool && check ctx e2 TBool 
                                        then Right TBool 
                                        else Left $ TypeError "Type mismatch in logical operation"   
-- TYP-NOT   
infer ctx (UnOp Not e)          = if check ctx e TBool 
                                    then Right TBool 
                                    else Left $ TypeError "Expected boolean for negation"   
infer _ _                       = Left $ TypeError "Unknown expression"

check :: Typ -> Exp -> Typ -> Bool
check ctx e tA                  = case infer ctx e of    
                                    Right tB -> tA == tB    
                                    _        -> False  
