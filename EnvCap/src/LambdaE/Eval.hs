module LambdaE.Eval where

import LambdaE.Syntax ( Op(..), Expr(..), Value )




-- When we perform projection
-- We need to deal with nitty gritty details of Projection
-- Step rules





-- Step will take a Expr and return a value

-- step :: Expr -> Value
-- step _ = 
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