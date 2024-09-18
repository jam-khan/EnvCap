module LambdaE.Eval where

import LambdaE.Syntax ( Op(..), Expr(..) )

isValue :: Expr -> Bool
isValue (Lit _)             = True
isValue Unit                = True
isValue (Clos v _ _)        = isValue v
isValue (Rec _ v)           = isValue v 
isValue (BinOp Mrg v1 v2)   = isValue v1 && isValue v2
isValue _                   = False
