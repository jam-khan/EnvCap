module LambdaE.SmallStep where
import LambdaE.Syntax ( Op(..), Expr(..), Value(..) )
import Data.Maybe (fromMaybe)


data LookupResultV = Found Value | NotFound
    deriving (Eq, Show)


lookupv :: Value -> Int -> Maybe Value
lookupv (VMrg v1 v2) 0 = Just v2
lookupv (VMrg v1 v2) n = lookupv v1 (n - 1)
lookupv _ _                 = Nothing


-- record lookup
rlookupv :: Value -> String -> Maybe Value
rlookupv (VRcd l e) label
    | l == label = Just e
rlookupv (VMrg v1 v2) label =
    case rlookupv v1 label of
        Just value  -> Just value
        Nothing     -> rlookupv v2 label
rlookupv _ _ = Nothing

evalS :: Expr -> Expr -> Maybe Value
evalS e expr = case evalSmall VUnit e of
                    Just v -> evalSmall v expr
                    _      -> Nothing


-- Big Step Evaluation
-- We assume that eval is gonna get environment as a value
evalSmall :: Value -> Expr -> Maybe Value
evalSmall env (Lit n)               = Just (VInt n)
evalSmall env Unit                  = Just VUnit
evalSmall env Ctx                   = Just env
evalSmall env 
    (BinOp App (Clos e2 t e) e1)  = evalSmall env (BinOp Box (BinOp Mrg e2 e1) e)
evalSmall env (BinOp App e1 e2)     = evalSmall (VMrg env v1) e2
                                    where Just v1 = evalSmall env e1
evalSmall env (BinOp Box e1 e2)     = evalSmall v1 e2
                                    where Just v1 = evalSmall env e1 
evalSmall env (BinOp Mrg e1 e2)     = Just (VMrg v1 v2)
                                    where   Just v1 = evalSmall env e1
                                            Just v2 = evalSmall (VMrg env v1) e2
evalSmall env (Lam t e)             = Just (VClos env t e)
evalSmall env (Proj e n)            = lookupv v1 n
                                    where   Just v1 = evalSmall env e
evalSmall env (RProj e s)           = rlookupv v1 s
                                    where   Just v1 = evalSmall env e
evalSmall env (Rec s e)             = case evalSmall env e of
                                        Just v -> Just (VRcd s v)
                                        _      -> Nothing
evalSmall _ _                       = Nothing