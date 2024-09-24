module LambdaE.Syntax where

import LambdaE.Types ( Typ(..) )
-- import qualified LambdaE.Types ( Typ ) -- Import Typ from Types module

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
data Value = VInt Expr               -- Integer value
        |    VUnit Expr              -- Unit value
        |    VClos Value Typ Expr    -- Closure
        |    VRcd String Value       -- Single-field record value
        |    VMrg Value Value        -- Merge of two values
        deriving (Eq, Show)

-- Checks on Syntax
-- Enforcing VInt Lit Int
isVInt :: Value -> Bool
isVInt (VInt ( Lit _ )) = True
isVInt _                = False

-- Enforcing VUnit Unit
isVUnit :: Value -> Bool
isVUnit (VUnit Unit)  = True
isVUnit _             = False

isValue :: Value -> Bool
isValue val = 
        case val of
                VInt _          -> isVInt val
                VUnit _         -> isVUnit val
                VClos v typ exp -> isValue v 
                VRcd label val  -> isValue val
                VMrg v1 v2      -> isValue v1 && isValue v2
