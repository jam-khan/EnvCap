module LambdaE.Eval where

import LambdaE.Syntax ( Op(..), Expr(..), Value )



{-


v.l --> v'                                  Selection

        SEL-REC
        ---------------         
        {l = v}.l --> v 





-}

data LookupResultV = Found Value | NotFound
    deriving (Eq, Show)

{-
,, is merge operator

    lookupv( v1 ,, v2 , 0)      = v2
    lookupv( v1 ,, v2, n + 1)   = lookupv (v1 , n) 
-}
lookupv :: Value -> Int -> Value
lookupv (VMrg v1 v2) 0 = v2
lookupv (VMrg v1 v2) n = lookupv v2 (n - 1)



step :: Expr -> Expr -> Expr

-- When we perform projection
-- We need to deal with nitty gritty details of Projection
-- Step rules

eval :: Value -> Expr -> Value 
eval context exp =
    case exp of
        Ctx             ->  context
        Unit            ->  VUnit
        Lit n           ->  VInt (Lit n)
        BinOp op e1 e2  ->  case exp of
                                BinOp App (Clos v1 A e) v2 ->
                                    eval context (BinOp Box (VMrg (eval context v1) (eval context v2)) e)
                                BinOp Box v1 e 
                                    -> eval context (BinOp Box (eval context v1) e) 

selectRec :: Expr -> Value  
selectRec (RProj (Rec label expr) l) =
    if label == l then expr

selectMRGL :: 



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