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
data Value = VInt                    -- Integer value
        |    VUnit                   -- Unit value
        |    VClos Value Typ Expr    -- Closure
        |    VRcd String Value       -- Single-field record value
        |    VMrg Value Value        -- Merge of two values
        deriving (Eq, Show)