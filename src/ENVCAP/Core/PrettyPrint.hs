module ENVCAP.Core.PrettyPrint where
import ENVCAP.Syntax

prettyPrintExp :: CoreTm -> String
prettyPrintExp Ctx              = "?"
prettyPrintExp Unit             = "ε"
prettyPrintExp (Lit i)          = show i
prettyPrintExp (EBool b)        = show b
prettyPrintExp (EString s)      = show s
prettyPrintExp (Lam typ e)      = "Lambda " ++ prettyPrintTyp typ ++  " { " ++ prettyPrintExp e ++ " }"
prettyPrintExp (Proj e n)       = prettyPrintExp e ++ "." ++ show n
prettyPrintExp (Clos e1 e2)     = "Clos<" ++ prettyPrintExp e1 ++ ", " ++ prettyPrintExp e2 ++ ">"
prettyPrintExp (Rec name e)     = "{ " ++ show name ++ " : " ++ prettyPrintExp e ++ " }"
prettyPrintExp (RProj e l)      = "(" ++ prettyPrintExp e ++ ")." ++ show l
prettyPrintExp (App e1 e2)      = "App[(" ++ prettyPrintExp e1 ++ ") " ++ prettyPrintExp e2 ++ "]"
prettyPrintExp (Mrg e1 e2)      = prettyPrintExp e1 ++ "\n ,, \n" ++ prettyPrintExp e2
prettyPrintExp (Box e1 e2)      = "Box[(" ++ prettyPrintExp e1 ++ " ▷ " ++ prettyPrintExp e2 ++ ")]"
prettyPrintExp (If e1 e2 e3)    = "If (" ++ prettyPrintExp e1 ++ ") then {" ++ prettyPrintExp e2 ++ " } else {" ++ prettyPrintExp e3 ++ "}"
prettyPrintExp (Fix ty e)       = "Fix (" ++ prettyPrintTyp ty ++ ") (\n" ++ prettyPrintExp e ++ "\n)"
prettyPrintExp (BinOp op e1 e2) = "(" ++ prettyPrintExp e1 ++ " " ++ show op ++ " " ++ prettyPrintExp e2 ++ ")"
prettyPrintExp (UnOp op e)      = show op ++ "(" ++ prettyPrintExp e ++ ")"
prettyPrintExp _                = "Nothing"

prettyPrintVal :: Value -> String
prettyPrintVal VUnit              = "ε"
prettyPrintVal (VInt i)           = show i
prettyPrintVal (VBool b)          = show b
prettyPrintVal (VString s)        = "\'" ++ show s ++ "\'"
prettyPrintVal (VClos val e)      = "closure<"  ++ prettyPrintVal val ++ ", "   ++ prettyPrintExp e ++ ">"
prettyPrintVal (VRcd label val)   = "{" ++ show label   ++ ": "     ++ prettyPrintVal val ++ "}"
prettyPrintVal (VMrg v1 v2)       = prettyPrintVal v1   ++ " ,, "   ++ prettyPrintVal v2
prettyPrintVal (VNil _)           = "List<[nil]>"
prettyPrintVal (VCons h rest)     = "List<["    ++ prettyPrintList' (VCons h rest)
prettyPrintVal (VPair v1 v2)      = "Pair[("    ++ prettyPrintVal v1    ++ ", "     ++ prettyPrintVal v2 ++ ")]"
prettyPrintVal (VInL typ value)   = "Left("     ++ show typ ++ ", "     ++ prettyPrintVal value ++ ")"
prettyPrintVal (VInR typ value)   = "Right("    ++ show typ ++ ", "     ++ prettyPrintVal value ++ ")"

prettyPrintList' :: Value -> String
prettyPrintList' (VCons h rest)     = prettyPrintVal h ++ ", " ++ prettyPrintList' rest 
prettyPrintList' (VNil _)           = "]>"
prettyPrintList' _                  = error "Shouldn't print anything except list"

prettyPrintTyp :: CoreTyp -> String
prettyPrintTyp TyCUnit                = "TYPE<ε>"
prettyPrintTyp TyCInt                 = "Int"
prettyPrintTyp TyCBool                = "Bool"
prettyPrintTyp TyCString              = "String"
prettyPrintTyp (TyCAnd t1 t2)         = prettyPrintTyp t1 ++ " & " ++ prettyPrintTyp t2
prettyPrintTyp (TyCArrow t1 t2)       = prettyPrintTyp t1 ++ " -> " ++ prettyPrintTyp t2
prettyPrintTyp (TyCRecord name typ)   = "{" ++ show name ++ " : " ++ prettyPrintTyp typ ++ "}"
prettyPrintTyp (TyCList typ)          = "List<" ++ prettyPrintTyp typ ++ ">"
prettyPrintTyp (TyCSum t1 t2)         = prettyPrintTyp t1 ++ " + " ++ prettyPrintTyp t2
prettyPrintTyp (TyCPair t1 t2)        = "(" ++ prettyPrintTyp t1 ++ ", " ++ prettyPrintTyp t2 ++ ")"