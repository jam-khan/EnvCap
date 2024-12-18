{-# LANGUAGE InstanceSigs #-}
module Core.Syntax where

-- Next Targets:
--      Finish Sums and Pairs
--      Design of the module system
--      Basic work on Parser

-- Extensions
-- Booleans         
        -- Semantics    : Done 
        -- Type System  : Done

-- Conditionals     
        -- Semantics    : Done
        -- Type System  : Done

-- Arithmetic
        -- Semantics    : Done
        -- Type System  : Done

-- Let bindings
        -- Semantics    : Done
        -- Type System  : Done

-- Recursion
        -- Semantics    : Done
        -- Type System  : Done

-- Built-in List
        -- Semantics    : Done
        -- Type System  : Done


-- Pairs
        -- Semantics    : To be done
        -- Type System (Product Types)  : To be done
-- Sums
        -- Semantics     : To be done
        -- Type Systems  : To be done


-- Operations Definitions
data BinaryOp   =       App             -- Application
                |       Box             -- Box
                |       Mrg             -- Merge
                --      Extensions
                |       Arith ArithOp   -- Arithmetic
                |       Comp  CompOp    -- CompOp
                |       Logic LogicOp   -- Boolean Logic

data Case1 = Temp [Exp]
        deriving (Eq, Show)

data Exp =  Ctx                     -- Context
        |   Unit                    -- Unit
        |   Lit    Int               -- Integer literal
        |   EBool  Bool              -- Boolean Term
        |   EString String           -- String Term
        |   BinOp  BinaryOp Exp Exp  -- Binary operations: Application, Box and Merge
        |   Lam    Typ Exp           -- Lambda Abstraction
        |   Proj   Exp Int           -- Projection
        |   Clos   Exp Exp       -- Closure
        |   Rec    String Exp        -- Single-Field Record
        |   RProj  Exp String        -- Record Projection by Label
        -- Extensions
        |   UnOp   UnaryOp Exp       -- Unary operations:  Not
        |   If     Exp Exp Exp       -- Conditionals
        |   Let    Exp Exp           -- Let Bindings
        |   Fix    Exp           -- Recursion
        -- Built-in Lists
        |   Pair Exp Exp
        |   Nil    Typ                -- Nil for list
        |   Cons   Exp Exp            -- List
        deriving Eq

-- Values
data Value =    VUnit                   -- Unit value
        |       VInt Int                -- Integer value
        |       VClos Value Exp         -- Closure
        |       VRcd String Value       -- Single-field record value
        |       VMrg Value Value        -- Merge of two values
        -- Extensions
        |       VFix Exp
        |       VBool Bool              -- Boolean Value
        |       VNil Typ                -- Nil for list
        |       VCons Value Value       -- List
        --      
        deriving Eq

-- Types
data Typ =  TUnit                  -- Unit type for empty environment
        |   TInt                   -- Integer type
        |   TString
        |   TAnd Typ Typ           -- Intersection type
        |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
        |   TRecord String Typ     -- Single-Field Record Type
        -- Extensions
        |   TBool                  -- Boolean type
        |   TList  Typ             -- Type for built-in list
        |   TSum   Typ Typ
        deriving Eq


data UnaryOp    =       Not
                |       Index Int
        deriving Eq

data ArithOp = Add | Sub | Mul | Div | Mod 
        deriving Eq
data CompOp  = Eql | Neq | Lt | Le | Gt | Ge
        deriving Eq
data LogicOp = And | Or
        deriving Eq

instance Show BinaryOp where
        show :: BinaryOp -> String
        show (Arith op) = show op
        show (Comp op)  = show op
        show (Logic op) = show op
        show App        = "App"
        show Box        = "\x25B8"
        show Mrg        = " ,, "

instance Show ArithOp where
        show :: ArithOp -> String
        show Add = "+"
        show Sub = "-"
        show Mul = "*"
        show Div = "/"
        show Mod = "%"

instance Show CompOp where
        show :: CompOp -> String
        show Eql = "=="
        show Neq = "!="
        show Lt  = "<"
        show Le  = "<="
        show Gt  = ">"
        show Ge  = ">="

instance Show LogicOp where
        show :: LogicOp -> String
        show And = "&&"
        show Or  = "||"

instance Show UnaryOp where
        show :: UnaryOp -> String
        show Not = "!"

instance Eq BinaryOp where 
        (==) :: BinaryOp -> BinaryOp -> Bool
        (Arith op1) == (Arith op2) = op1 == op2
        (Comp op1)  == (Comp op2)  = op1 == op2
        (Logic op1) == (Logic op2) = op1 == op2
        App         == App         = True
        Box         == Box         = True
        Mrg         == Mrg         = True
        _           == _           = False


isValue :: Value -> Bool
isValue VUnit                   = True
isValue (VInt _)                = True
isValue (VBool _)               = True
isValue (VClos v e)             = isValue v
isValue (VRcd label val)        = isValue val
isValue (VMrg v1 v2)            = isValue v1 && isValue v2


instance Show Exp where
        show :: Exp -> String
        show                = showIndented 0

showIndented :: Int -> Exp -> String
showIndented n exp = indent 0 ++ show' n exp
        where
                indent :: Int -> String
                indent n1 = replicate n1 ' '

                show' :: Int -> Exp -> String
                show' _ Ctx                = "?"
                show' _ Unit               = "\x03B5"
                show' _ (Lit i)            = show i
                show' n (BinOp op e1 e2)   = "(" ++ show' n e1 ++ " " ++ show op ++ " " ++ show' n e2 ++ ")"
                show' n (UnOp op e)        = show op ++ "(" ++ show' n e ++ ")"
                show' n (Lam typ e)        =  "\n"
                                                ++ indent n ++ "\x03BB" ++ " " ++ show typ ++ " . \n" 
                                                        ++ indent (n + 2) ++ show' (n + 2) e
                show' n (Proj e n')        = show' n e ++ "." ++ show n'
                show' n (Clos e1 e2)       = "< " ++ show' n e1 ++ ", " ++ show' (n + 2) e2 ++ " >"
                show' n (Rec s e)          = "{ " ++ show s  ++ " = " ++ show' (n + 2) e ++ " }"
                show' n (RProj e s)        = show' n e ++ "." ++ show s
                show' n (If c e1 e2)       = "IF " ++ show' (n + 2) c ++ "\n" 
                                                ++ indent (n + 4) ++ "THEN \n" ++ indent (n + 6) ++ show' (n + 4) e1 ++ "\n" 
                                                ++ indent (n + 4) ++ "ELSE \n" ++ indent (n + 6) ++ show' (n + 4) e2 ++ "\n"
                show' n (Let e1 e2)        =    "LET \n" 
                                                        ++ indent n ++ show' n e1 ++
                                                "\n IN " 
                                                        ++ indent n ++ show' (n + 2) e2
                show' _ (EBool b)          = show b
                show' n (Fix e)            = indent n ++ "fix " ++ show' (n + 2) e
                show' n (Pair e1 e2)       = "(" ++ show' n e1 ++ ", " ++ show' n e2 ++ ")"
                show' _ (Nil typ)          = indent n ++ "NIL of " ++ show typ
                show' n (Cons e1 e2)       = indent n ++ show' n e1 ++ " :: " ++ show' n e2


instance Show Typ where 
        show :: Typ -> String
        show TUnit              = "\x03B5"
        show TInt               = "INT"
        show TBool              = "BOOL"
        show (TAnd t1 t2)       = show t1 ++ " & " ++ show t2
        show (TArrow t1 t2)     = show t1 ++ " -> " ++ show t2
        show (TRecord s t)      = "{ " ++ show s ++ " :: " ++ show t ++ " }"
        show (TList typ)        = "[" ++ show typ ++ "]"
        
instance Show Value where
        show :: Value -> String
        show VUnit                      = "\x03B5"
        show (VInt i)                   = show i
        show (VBool b)                  = show b
        show (VClos val exp)        =   "< { " ++ show val ++ " }, \n(" ++ showIndented 6 exp ++ "}"
        show (VRcd label val)           = "{ " ++ show label ++ " = "  ++ show val ++ " }"             
        show (VMrg v1 v2)               = show v1 ++ " ,, " ++ show v2
        show (VNil typ)                 = "nil of " ++ show typ
        show (VCons head rest)          = show head ++ " :: " ++ show rest