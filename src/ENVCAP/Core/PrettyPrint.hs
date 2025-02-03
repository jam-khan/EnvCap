module ENVCAP.Core.PrettyPrint where

import ENVCAP.Core.Syntax

prettyPrintExp :: Exp -> String
-- data Exp =  Ctx                      -- Context
prettyPrintExp Ctx              = "?"
--         |   Unit                     -- Unit
prettyPrintExp Unit             = "ε"
--         |   Lit    Integer           -- Integer literal
prettyPrintExp (Lit i)          = show i
--         |   EBool   Bool             -- Boolean Term
prettyPrintExp (EBool b)        = show b
--         |   EString String           -- String Term
prettyPrintExp (EString s)      = show s
--         |   Lam    Typ Exp           -- Lambda Abstraction
prettyPrintExp (Lam typ exp)    = "Lambda " ++ prettyPrintTyp typ ++  " { " ++ prettyPrintExp exp ++ " }"
--         |   Proj   Exp Int           -- Projection
prettyPrintExp (Proj e n)       = prettyPrintExp e ++ "." ++ show n
--         |   Clos   Exp Exp           -- Closure
prettyPrintExp (Clos e1 e2)     = "Clos<" ++ prettyPrintExp e1 ++ ", " ++ prettyPrintExp e2 ++ ">"
--         |   Rec    String Exp        -- Single-Field Record
prettyPrintExp (Rec name exp)   = "{ " ++ show name ++ " : " ++ prettyPrintExp exp ++ " }"
--         |   RProj  Exp String        -- Record Projection by Label
prettyPrintExp (RProj e l)      = "(" ++ prettyPrintExp e ++ ")." ++ show l
--         |   App    Exp Exp           -- Application
prettyPrintExp (App e1 e2)      = "App[(" ++ prettyPrintExp e1 ++ ") " ++ prettyPrintExp e2 ++ "]"
--         |   Mrg    Exp Exp           -- Merge
prettyPrintExp (Mrg e1 e2)      = prettyPrintExp e1 ++ "\n ,, \n" ++ prettyPrintExp e2
--         |   Box    Exp Exp           -- Box
prettyPrintExp (Box e1 e2)      = "Box[(" ++ prettyPrintExp e1 ++ " ▷ " ++ prettyPrintExp e2 ++ ")]"
--         |   If     Exp Exp Exp       -- Conditionals
prettyPrintExp (If e1 e2 e3)    = "If (" ++ prettyPrintExp e1 ++ ") then {" ++ prettyPrintExp e2 ++ " } else {" ++ prettyPrintExp e3 ++ "}"
--         |   Let    Exp Exp           -- Let Bindings
prettyPrintExp (Let e1 e2)      = "Let (" ++ prettyPrintExp e1 ++ ") in {\n" ++ prettyPrintExp e2 ++ " }"
--         |   Fix    Exp               -- Recursion
prettyPrintExp (Fix e)          = "Fix (\n" ++ prettyPrintExp e ++ "\n)"
--         |   Pair   Exp Exp           -- Pair
prettyPrintExp (Pair e1 e2)     = "(" ++ prettyPrintExp e1 ++ ", " ++ prettyPrintExp e2 ++ ")"
--         |   Fst    Exp               -- First Projection
prettyPrintExp (Fst e)          = "Fst(" ++ prettyPrintExp e ++ ")"
--         |   Snd    Exp               -- Second Projection
prettyPrintExp (Snd e)          = "Snd(" ++ prettyPrintExp e ++ ")"
--         |   InL    Typ Exp           -- Tagging Left
prettyPrintExp (InL typ e)      = "Left(" ++ prettyPrintTyp typ ++ ", " ++ prettyPrintExp e ++ ")"
--         |   InR    Typ Exp           -- Tagging Right
prettyPrintExp (InR typ e)      = "Right(" ++ prettyPrintTyp typ ++ ", " ++ prettyPrintExp e ++ ")"       
--         -- Sums
prettyPrintExp (Case e1 e2 e3)  = "Case (" ++ prettyPrintExp e1 ++ ") of \n" ++
                                            " inl _ _ => " ++ prettyPrintExp e2 ++ "\n" ++
                                            " inr _ _ => " ++ prettyPrintExp e3 ++ "\n"
prettyPrintExp (Nil tA)         = "List<[" ++ show tA ++"]>"
--         |   Cons   Exp Exp           -- Cons for List
prettyPrintExp (Cons head rest) = "List<[" ++ prettyPrintList (Cons head rest)
--         |   BinOp  BinaryOp Exp Exp  -- Binary operations
prettyPrintExp (BinOp op e1 e2) = "(" ++ prettyPrintExp e1 ++ " " ++ show op ++ " " ++ prettyPrintExp e2 ++ ")"
--         |   UnOp   UnaryOp Exp       -- Unary operations
prettyPrintExp (UnOp op e)      = show op ++ "(" ++ prettyPrintExp e ++ ")"
--         |   LCase  Exp Exp Exp       -- Case of List
prettyPrintExp (LCase e1 e2 e3) = "Case (" ++ prettyPrintExp e1 ++ ") of \n" ++
                                            " []        => " ++ prettyPrintExp e2 ++ "\n" ++
                                            " (x:xs)    => " ++ prettyPrintExp e3 ++ "\n"


prettyPrintList :: Exp -> String
prettyPrintList (Cons head rest)    = prettyPrintExp head ++ ", " ++ prettyPrintList rest 
prettyPrintList (Nil tA)            = "]>"

prettyPrintVal :: Value -> String
prettyPrintVal VUnit              = "ε"
prettyPrintVal (VInt i)           = show i
prettyPrintVal (VBool b)          = show b
prettyPrintVal (VString s)        = "\'" ++ show s ++ "\'"
prettyPrintVal (VClos val exp)    = "closure<"  ++ prettyPrintVal val ++ ", "   ++ prettyPrintExp exp ++ ">"
prettyPrintVal (VRcd label val)   = "{" ++ show label   ++ ": "     ++ prettyPrintVal val ++ "}"
prettyPrintVal (VMrg v1 v2)       = prettyPrintVal v1   ++ " ,, "   ++ prettyPrintVal v2
prettyPrintVal (VNil tA)          = "List<[nil]>"
prettyPrintVal (VCons head rest)  = "List<["    ++ prettyPrintList' (VCons head rest)
prettyPrintVal (VPair v1 v2)      = "Pair[("    ++ prettyPrintVal v1    ++ ", "     ++ prettyPrintVal v2 ++ ")]"
prettyPrintVal (VInL typ value)   = "Left("     ++ show typ ++ ", "     ++ prettyPrintVal value ++ ")"
prettyPrintVal (VInR typ value)   = "Right("    ++ show typ ++ ", "     ++ prettyPrintVal value ++ ")"

prettyPrintList' :: Value -> String
prettyPrintList' (VCons head rest)   = prettyPrintVal head ++ ", " ++ prettyPrintList' rest 
prettyPrintList' (VNil tA)           = "]>"

prettyPrintTyp :: Typ -> String
prettyPrintTyp TUnit                = "TYPE<ε>"
prettyPrintTyp TInt                 = "Int"
prettyPrintTyp TBool                = "Bool"
prettyPrintTyp TString              = "String"
prettyPrintTyp (TAnd t1 t2)         = prettyPrintTyp t1 ++ " & " ++ prettyPrintTyp t2
prettyPrintTyp (TArrow t1 t2)       = prettyPrintTyp t1 ++ " -> " ++ prettyPrintTyp t2
prettyPrintTyp (TRecord name typ)   = "{" ++ show name ++ " : " ++ prettyPrintTyp typ ++ "}"
prettyPrintTyp (TList typ)          = "List<" ++ prettyPrintTyp typ ++ ">"
prettyPrintTyp (TSum t1 t2)         = prettyPrintTyp t1 ++ " + " ++ prettyPrintTyp t2
prettyPrintTyp (TPair t1 t2)        = "(" ++ prettyPrintTyp t1 ++ ", " ++ prettyPrintTyp t2 ++ ")"