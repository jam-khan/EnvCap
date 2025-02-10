{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
module ENVCAP.Core.TypeChecker where
import ENVCAP.Core.Syntax (Exp(..), Typ(..), BinaryOp(..), UnaryOp(..))
import ENVCAP.Core.Util (rlookupt, lookupt, containment)

data TypeError = TypeError String deriving Show

infer :: Typ -> Exp -> Either TypeError Typ
infer ctx Ctx                 = Right ctx
infer _ Unit                  = Right TUnit
infer _ (Lit _)               = Right TInt
infer _ (EBool _)             = Right TBool
infer _ (EString _)           = Right TString
infer ctx (Proj e n)          = infer ctx e >>= \tB -> case lookupt tB n of
                                    Just t  -> Right t
                                    Nothing -> Left $ TypeError $ "Projection " ++ show n 
                                                                        ++ " failed on type " ++ show tB  ++ " ctx: "  ++ show ctx
infer ctx (Box e1 e2)           = infer ctx e1 >>= \ctx1 -> infer ctx1 e2
infer ctx (Mrg e1 e2)           = infer ctx e1 >>= \tA -> infer (TAnd ctx tA) e2 >>= \tB -> Right (TAnd tA tB)
infer ctx (App e1 e2)           = infer ctx e1 >>= \ty1 -> 
                                    case ty1 of
                                        TArrow tA tB -> if check ctx e2 tA  then Right tB    
                                                                            else Left $ TypeError ("Type mismatch in application: Context:" ++ show ctx)
                                        _            -> Left $ 
                                                            TypeError ("Expected a function type in application Function Type: " 
                                                                        ++ show ty1 ++ " Function: " ++ show e1)
infer ctx (Lam tA e)            = infer (TAnd ctx tA) e >>= \tB -> Right (TArrow tA tB)
infer ctx (Clos e1 (Lam tA e2)) = infer ctx e1 >>= \ctx1 -> infer (TAnd ctx1 tA) e2 >>= \tB -> Right (TArrow tA tB)
infer ctx (Rec l e)             = infer ctx e >>= \tA -> Right (TRecord l tA)
infer ctx (RProj e l)           = infer ctx e >>= \tB ->
                                    case rlookupt tB l of 
                                        Just tA -> if containment (TRecord l tA) tB 
                                                then Right tA 
                                                else Left $ TypeError "Record projection failed due to containment"
                                        Nothing -> Left $ TypeError $ "Field " ++ show l ++ " not found in type " ++ show tB 
infer ctx (Fix tA e)            = if check (TAnd ctx tA) e tA then Right tA else Left $ TypeError "Fixpoint type check failed"
infer ctx (If cond e1 e2)       = if check ctx cond TBool 
                                        then infer ctx e1 >>= \t1 -> 
                                                infer ctx e2 >>= \t2 -> 
                                                    if t1 == t2 then Right t1 
                                                                else Left $ TypeError "Branches of if must have the same type"  
                                        else Left $ TypeError "Condition must be of type Bool"
infer ctx (Pair e1 e2)          = infer ctx e1 >>= \ta -> infer ctx e2 >>= \tb -> Right $ TPair ta tb
infer ctx (Fst e)               = infer ctx e >>= \case TPair ta _ -> Right ta 
                                                        _          -> Left $ TypeError "Expected a pair type"
infer ctx (Snd e)               = infer ctx e >>= \case TPair _ tb -> Right tb 
                                                        _           -> Left $ TypeError "Expected a pair type"
infer ctx (InL tb e1)           = infer ctx e1 >>= \ta -> Right $ TSum ta tb  
infer ctx (InR ta e2)           = infer ctx e2 >>= \tb -> Right $ TSum ta tb
infer ctx (Case e1 e2 e3)       = infer ctx e1 >>= \case 
                                                    TSum ta tb -> 
                                                        infer (TAnd ctx ta) e2 >>= \te2 ->
                                                                    if check (TAnd ctx tb) e3 te2 
                                                                        then Right te2 
                                                                        else Left $ TypeError "Case branches must match"
                                                    _ -> Left $ TypeError "Expected a sum type in case expression"
infer _ (Nil typ)               = Right (TList typ)
infer ctx (Cons te te2)         = case infer ctx te2 of
                                    Right (TList tA) -> if check ctx te tA  then Right (TList tA)
                                                                            else Left $ TypeError "Type mismatch in list constructor"
                                    _                           -> Left $ TypeError "Expected a list type or Null type"
infer ctx (LCase e1 e2 e3)      = infer ctx e1 >>= \case 
                                        TList _ -> infer ctx e2 >>= \case 
                                                        TList tb -> if check ctx e3 tb 
                                                                        then Right tb
                                                                        else Left $ TypeError "Type mismatch in list case branches"
                                                        _ -> Left $ TypeError "Expected a list type for second branch"  
                                        _       -> Left $ TypeError "Expected a list type for first branch"  
infer ctx (BinOp (Arith _) e1 e2) 
                                = infer ctx e1 >>= \t1 ->
                                        infer ctx e2 >>= \t2 ->
                                            if t1 == TInt && check ctx e2 TInt  
                                                then Right TInt 
                                                else Left $ TypeError ("Type mismatch in arithmetic operation " ++ show t1 ++ " " ++ show t2)
infer ctx (BinOp (Comp _) e1 e2) 
                                =  infer ctx e1 >>= \t1 -> if check ctx e2 t1
                                                                then Right TBool 
                                                                else Left $ 
                                                                    TypeError "Type mismatch in comparison operation. Expected an integer, boolean, or string for comparison" 
infer ctx (BinOp (Logic _) e1 e2) 
                                = infer ctx e1 >>= \t1 ->   if t1 == TBool && check ctx e2 TBool 
                                                                then Right TBool 
                                                                else Left $ TypeError "Type mismatch in logical operation"   
infer ctx (UnOp Not e)          = if check ctx e TBool  then Right TBool 
                                                        else Left $ TypeError "Expected boolean for negation"   
infer _ _                       = Left $ TypeError "Unknown expression"

check :: Typ -> Exp -> Typ -> Bool
check ctx e tA                  = case infer ctx e of    
                                    Right tB -> tA == tB    
                                    _        -> False  
