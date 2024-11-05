{-# LANGUAGE InstanceSigs #-}
module Core.Syntax where

-- Operations Definitions
data BinaryOp   =       App             -- Application
                |       Box             -- Box
                |       Mrg             -- Merge
                |       Arith ArithOp   -- Arithmetic
                |       Comp  CompOp    -- CompOp
                |       Logic LogicOp   -- Boolean Logic

data UnaryOp    =       Not
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
        show Box        = "Box"
        show Mrg        = "Mrg"

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

-- Extensions
    -- Booleans         -- Done -- Type Checker
    -- Conditionals     -- Done -- Type Checker
    -- Arithmetic       -- Done -- Type Checker
    -- Let bindings     -- Sortof (Just sugar)

    -- Recursion        -- X

    
    -- Builtin lists
    -- Pairs
    -- Algebraic Datatypes (Without Polymorphism)

data Exp =  Ctx                     -- Context
        |   Unit                    -- Unit
        |   Lit   Int               -- Integer literal
        |   BinOp BinaryOp Exp Exp  -- Binary operations: Application, Box and Merge
        |   UnOp  UnaryOp Exp       -- Unary operations:  Not
        |   Lam   Typ Exp           -- Lambda Abstraction
        |   Proj  Exp Int           -- Projection
        |   Clos  Exp Typ Exp       -- Closure
        |   Rec   String Exp        -- Single-Field Record
        |   RProj Exp String        -- Record Projection by Label
        -- Extensions
        |   Let   Exp Exp           -- Let Bindings
        |   EBool Bool              -- Boolean Term
        |   If    Exp Exp Exp       -- Conditionals
        deriving (Eq, Show)

-- Values
data Value =    VUnit                   -- Unit value
        |       VInt Int                -- Integer value
        |       VBool Bool 
        |       VClos Value Typ Exp     -- Closure
        |       VRcd String Value       -- Single-field record value
        |       VMrg Value Value        -- Merge of two values
        deriving (Eq, Show)


data Typ =  TUnit                  -- Unit type for empty environment
        |   TInt                   -- Integer type
        |   TBool                  -- Boolean type
        |   TAnd Typ Typ           -- Intersection type
        |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
        |   TRecord String Typ     -- Single-Field Record Type
        |   TArray Typ
        deriving (Eq, Show)

isValue :: Value -> Bool
isValue VUnit                   = True
isValue (VInt _)                = True
isValue (VBool _)               = True
isValue (VClos v t e)           = isValue v
isValue (VRcd label val)        = isValue val
isValue (VMrg v1 v2)            = isValue v1 && isValue v2
