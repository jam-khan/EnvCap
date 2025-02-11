{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
module ENVCAP.Core.TypeChecker where
import ENVCAP.Syntax (Exp(..), TypC(..), BinaryOp(..), UnaryOp(..))
import ENVCAP.Core.Util (rlookupt, lookupt, containment)

data TypeError = TypeError String deriving Show

infer :: TypC -> Exp -> Either TypeError TypC
infer ctx Ctx                 = Right ctx
infer _ Unit                  = Right TyCUnit
infer _ (Lit _)               = Right TyCInt
infer _ (EBool _)             = Right TyCBool
infer _ (EString _)           = Right TyCString
infer ctx (Proj e n)          = infer ctx e >>= \tB -> 
                                case lookupt tB n of
                                    Just t  -> Right t
                                    Nothing -> Left $ TypeError $ "Projection " ++ show n 
                                                                        ++ " failed on type " ++ show tB  ++ " ctx: "  ++ show ctx
infer ctx (Box e1 e2)           = infer ctx e1 >>= \ctx1 -> infer ctx1 e2
infer ctx (Mrg e1 e2)           = infer ctx e1 >>= \tA -> infer (TyCAnd ctx tA) e2 >>= \tB -> Right (TyCAnd tA tB)
infer ctx (App e1 e2)           = infer ctx e1 >>= \ty1 -> 
                                    case ty1 of
                                        TyCArrow tA tB -> if check ctx e2 tA  then Right tB    
                                                                            else Left $ TypeError ("Type mismatch in application: Context:" ++ show ctx)
                                        _            -> Left $ 
                                                            TypeError ("Expected a function type in application Function Type: " 
                                                                        ++ show ty1 ++ " Function: " ++ show e1)
infer ctx (Lam tA e)            = infer (TyCAnd ctx tA) e >>= \tB -> Right (TyCArrow tA tB)
infer ctx (Clos e1 (Lam tA e2)) = infer ctx e1 >>= \ctx1 -> 
                                    infer (TyCAnd ctx1 tA) e2 >>= \tB -> 
                                        Right (TyCArrow tA tB)
infer ctx (Rec l e)             = infer ctx e >>= \tA -> Right (TyCRecord l tA)
infer ctx (RProj e l)           = infer ctx e >>= \tB ->
                                    case rlookupt tB l of 
                                        Just tA -> if containment (TyCRecord l tA) tB 
                                                then Right tA 
                                                else Left $ TypeError "Record projection failed due to containment"
                                        Nothing -> Left $ TypeError $ "Field " ++ show l ++ " not found in type " ++ show tB 
infer ctx (Fix tA e)            = if check (TyCAnd ctx tA) e tA then Right tA else Left $ TypeError "Fixpoint type check failed"
infer ctx (If cond e1 e2)       = if check ctx cond TyCBool 
                                        then infer ctx e1 >>= \t1 -> 
                                                infer ctx e2 >>= \t2 -> 
                                                    if t1 == t2 then Right t1 
                                                                else Left $ TypeError "Branches of if must have the same type"  
                                        else Left $ TypeError "Condition must be of type Bool"
infer ctx (Pair e1 e2)          = infer ctx e1 >>= \ta -> infer ctx e2 >>= \tb -> Right $ TyCPair ta tb
infer ctx (Fst e)               = infer ctx e >>= \case TyCPair ta _ -> Right ta 
                                                        _          -> Left $ TypeError "Expected a pair type"
infer ctx (Snd e)               = infer ctx e >>= \case TyCPair _ tb -> Right tb 
                                                        _           -> Left $ TypeError "Expected a pair type"
infer ctx (InL tb e1)           = infer ctx e1 >>= \ta -> Right $ TyCSum ta tb  
infer ctx (InR ta e2)           = infer ctx e2 >>= \tb -> Right $ TyCSum ta tb
infer ctx (Case e1 e2 e3)       = infer ctx e1 >>= \case 
                                                    TyCSum ta tb -> 
                                                        infer (TyCAnd ctx ta) e2 >>= \te2 ->
                                                                    if check (TyCAnd ctx tb) e3 te2 
                                                                        then Right te2 
                                                                        else Left $ TypeError "Case branches must match"
                                                    _ -> Left $ TypeError "Expected a sum type in case expression"
infer _ (Nil typ)               = Right (TyCList typ)
infer ctx (Cons te te2)         = case infer ctx te2 of
                                    Right (TyCList tA) -> if check ctx te tA  then Right (TyCList tA)
                                                                            else Left $ TypeError "Type mismatch in list constructor"
                                    _                           -> Left $ TypeError "Expected a list type or Null type"
infer ctx (LCase e1 e2 e3)      = infer ctx e1 >>= \case 
                                        TyCList _ -> infer ctx e2 >>= \case 
                                                        TyCList tb -> if check ctx e3 tb 
                                                                        then Right tb
                                                                        else Left $ TypeError "Type mismatch in list case branches"
                                                        _ -> Left $ TypeError "Expected a list type for second branch"  
                                        _       -> Left $ TypeError "Expected a list type for first branch"  
infer ctx (BinOp (Arith _) e1 e2) 
                                = infer ctx e1 >>= \t1 ->
                                        infer ctx e2 >>= \t2 ->
                                            if t1 == TyCInt && check ctx e2 TyCInt  
                                                then Right TyCInt 
                                                else Left $ TypeError ("Type mismatch in arithmetic operation " ++ show t1 ++ " " ++ show t2)
infer ctx (BinOp (Comp _) e1 e2) 
                                =  infer ctx e1 >>= \t1 -> if check ctx e2 t1
                                                                then Right TyCBool 
                                                                else Left $ 
                                                                    TypeError "Type mismatch in comparison operation. Expected an integer, boolean, or string for comparison" 
infer ctx (BinOp (Logic _) e1 e2) 
                                = infer ctx e1 >>= \t1 ->   if t1 == TyCBool && check ctx e2 TyCBool 
                                                                then Right TyCBool
                                                                else Left $ TypeError "Type mismatch in logical operation"   
infer ctx (UnOp Not e)          = if check ctx e TyCBool  then Right TyCBool 
                                                        else Left $ TypeError "Expected boolean for negation"   
infer _ _                       = Left $ TypeError "Unknown expression"

check :: TypC -> Exp -> TypC -> Bool
check ctx e tA                  = case infer ctx e of    
                                    Right tB -> tA == tB    
                                    _        -> False  
