module LambdaE.Eval where

import LambdaE.Syntax ( Op(..), Expr(..), Value )

isValue :: Expr -> Bool
isValue (Lit _)             = True
isValue Unit                = True
isValue (Clos v _ _)        = isValue v
isValue (Rec _ v)           = isValue v 
isValue (BinOp Mrg v1 v2)   = isValue v1 && isValue v2
isValue _                   = False



-- When we perform projection
-- We need to deal with nitty gritty details of Projection




-- Step will take a Expr and return a value

-- step :: Expr -> Value
-- step :: Expr -> Value 

-- data Expr = Ctx                     -- Context
--         |   Unit                    -- Unit
--         |   Lit Int                 -- Integer literal
--         |   BinOp Op Expr Expr      -- Binary operations: Application, Box and Merge
--         |   Lam Typ Expr            -- Lambda Abstraction
--         |   Proj Expr Int           -- Projection
--         |   Clos Expr Typ Expr      -- Closure
--         |   Rec  String Expr        -- Single-Field Record
--         |   RProj Expr String       -- Record Projection by Label
--         deriving (Eq, Show)

-- step :: Expr -> Maybe Expr
-- step :: 