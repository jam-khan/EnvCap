module ENVCAP.Core.Util where

import ENVCAP.Syntax (BinaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), TypC (..))


arithOp :: ArithOp -> Integer -> Integer -> Maybe Integer
arithOp Add v1 v2 = Just (v1 + v2)
arithOp Sub v1 v2 = Just (v1 - v2)
arithOp Mul v1 v2 = Just (v1 * v2)
arithOp Div v1 v2 = if v2 == 0 then Nothing else Just (v1 `Prelude.div` v2)
arithOp Mod v1 v2 = if v2 == 0 then Nothing else Just (v1 `Prelude.mod` v2)

lookupv :: Value -> Int -> Maybe Value
lookupv (VMrg _  v2) 0 = Just v2
lookupv (VMrg v1 _ ) n = lookupv v1 (n - 1)
lookupv _ _            = Nothing

rlookupv :: Value -> String -> Maybe Value
rlookupv (VRcd l v) label
    | l == label = Just v
rlookupv (VMrg v1 v2) label =
    case (rlookupv v1 label, rlookupv v2 label) of
        (Just vL, Nothing)      -> Just vL
        (Nothing, Just vR)      -> Just vR
        (_, _)                  -> Nothing
rlookupv _ _ = Nothing

lookupt :: TypC -> Int -> Maybe TypC
lookupt (TyCAnd _ tB) 0         = Just tB
lookupt (TyCAnd tA _) n         = lookupt tA (n - 1)
lookupt _ _                     = Nothing

isLabel :: String -> TypC -> Bool
isLabel l (TyCRecord label _)   = l == label
isLabel l (TyCAnd tA tB)        = isLabel l tA || isLabel l tB
isLabel _ _                     = False

containment :: TypC -> TypC -> Bool
containment (TyCRecord l tA) (TyCRecord label typ ) 
                                = l == label && tA == typ
containment (TyCRecord l tA) (TyCAnd tB tC) 
                                =   (containment (TyCRecord l tA) tB && not (isLabel l tC)) ||
                                    (containment (TyCRecord l tA) tC && not (isLabel l tB))
containment _ _                 = False

rlookupt :: TypC -> String -> Maybe TypC
rlookupt (TyCRecord l t) label
    | l == label = Just t
rlookupt (TyCAnd tA tB) label = case rlookupt tB label of
                                Just t    -> Just t
                                Nothing   -> rlookupt tA label
rlookupt _ _                = Nothing

compareOp :: CompOp -> Value -> Value -> Bool
compareOp op  (VInt v1) (VInt v2)       
                                = compareWith op v1 v2
compareOp op  (VBool b1) (VBool b2)     
                                = compareWith op b1 b2
compareOp op  (VString s1) (VString s2) 
                                = compareWith op s1 s2
compareOp _ _ _                 = False


compareWith :: (Ord a) => CompOp -> a -> a -> Bool
compareWith Eql  x y            =   x == y
compareWith Neq x y             =   x /= y
compareWith Lt  x y             =   x < y
compareWith Le  x y             =   x <= y
compareWith Gt  x y             =   x > y
compareWith Ge  x y             =   x >= y


box :: Exp -> Exp -> Exp
box = Box

merge :: Exp -> Exp -> Exp
merge = Mrg

proj :: Int -> Exp
proj = Proj Ctx

add :: Exp -> Exp -> Exp
add = BinOp (Arith Add)

sub :: Exp -> Exp -> Exp
sub = BinOp (Arith Sub)

mult :: Exp -> Exp -> Exp
mult = BinOp (Arith Mul)

div :: Exp -> Exp -> Exp
div = BinOp (Arith Div)

mod :: Exp -> Exp -> Exp
mod = BinOp (Arith Mod)

apply :: Exp -> Exp -> Exp
apply = App