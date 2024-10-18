module LambdaE.Syntax where

-- Operations Definitions
data Op =   App -- Application
        |   Box -- Box
        |   Mrg -- Merge
        deriving (Eq, Show)


data Expr = Ctx                     -- Context
        |   Unit                    -- Unit
        |   Lit Int                 -- Integer literal
        |   BinOp Op Expr Expr      -- Binary operations: Application, Box and Merge
        |   Lam Typ Expr            -- Lambda Abstraction
        |   Proj Expr Int           -- Projection
        |   Clos Expr Typ Expr      -- Closure
        |   Rec  String Expr        -- Single-Field Record
        |   RProj Expr String       -- Record Projection by Label
        deriving (Eq, Show)


-- Values
data Value =    VUnit                   -- Unit value
        |       VInt Int                -- Integer value
        |       VClos Value Typ Expr    -- Closure
        |       VRcd String Value       -- Single-field record value
        |       VMrg Value Value        -- Merge of two values
        deriving (Eq, Show)


data Typ = 
        TInt                   -- Integer type
    |   TUnit                  -- Unit type for empty environment
    |   TAnd Typ Typ           -- Intersection type
    |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
    |   TRecord String Typ     -- Single-Field Record Type         
    deriving (Eq, Show)

isValue :: Value -> Bool
isValue VUnit                   = True
isValue (VInt _)                = True
isValue (VClos v t e)           = isValue v
isValue (VRcd label val)        = isValue val
isValue (VMrg v1 v2)            = isValue v1 && isValue v2
