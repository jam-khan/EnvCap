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
    -- Booleans         
        -- Semantics    : Done 
        -- Type System  : To be done
    -- Conditionals     
        -- Semantics    : Done
        -- Type System  : To be done
    -- Arithmetic
        -- Semantics    : Done
        -- Type System  : To be done 
    -- Let bindings
        -- Semantics    : Done
        -- Type System  : To be done
    -- Built-in List

    -- Arithmetic       -- Done -- Type Checker
    -- Let bindings     -- Sortof (Just sugar)
    -- Pairs            -- Done
    -- Builtin lists
    -- Pairs
    -- Sums
    -- Recursion

    -- Algebraic Datatypes (Without Polymorphism)
-- Product Types
-- Simply add the pairs by sugar

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
        |   If    Exp Exp Exp       -- Conditionals
        |   Let   Exp Exp           -- Let Bindings
        |   EBool Bool              -- Boolean Term
        |   Fix   Exp               -- Recursion
        |   Pair  Exp Exp           -- Pair
        |   Nil  Typ                -- Nil for list
        |   Cons Exp Exp            -- List
        -- |      Sums
        deriving (Eq, Show)

-- Values
data Value =    VUnit                   -- Unit value
        |       VInt Int                -- Integer value
        |       VBool Bool 
        |       VClos Value Typ Exp     -- Closure
        |       VRcd String Value       -- Single-field record value
        |       VMrg Value Value        -- Merge of two values
        |       VNil Typ                -- Nil for list
        |       VCons Value Value        -- List
        deriving (Eq, Show)


data Typ =  TUnit                  -- Unit type for empty environment
        |   TInt                   -- Integer type
        |   TBool                  -- Boolean type
        |   TAnd Typ Typ           -- Intersection type
        |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
        |   TRecord String Typ     -- Single-Field Record Type
        |   TList  Typ             -- Type for built-in list
        |   TFix   Typ             -- Type for recursive function 
        deriving (Eq, Show)

{-
        Extension Typing Rules

        v |- e1 <= Bool         v |- e2 => t           v |- e3 <= t
        ----------------------------------------------------------- (T-IF)
                        v |- If e1 e2 e3 => t1


-}





isValue :: Value -> Bool
isValue VUnit                   = True
isValue (VInt _)                = True
isValue (VBool _)               = True
isValue (VClos v t e)           = isValue v
isValue (VRcd label val)        = isValue val
isValue (VMrg v1 v2)            = isValue v1 && isValue v2
