module Core.Util where

import Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import Data.Maybe (fromMaybe)


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

