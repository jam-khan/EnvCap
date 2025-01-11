module ENVCAP.Core.Examples.Lists where
import ENVCAP.Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import ENVCAP.Core.Util  (apply, proj, sub, mult, add)
import ENVCAP.Core.Evaluator (eval)

simpleList :: Exp
simpleList = Cons (Lit 1) (Cons (Lit 2) (Cons (Lit 3) (Nil TInt)))

simpleLCase :: Exp
simpleLCase = LCase
                (Cons (Lit 1) (Cons (Lit 2) (Nil TInt)))
                (proj 0)
                (proj 1)

complexLCase :: Exp
complexLCase    = LCase
                    (Cons (Lit 1) (Cons (Lit 2) (Nil TInt)))
                    (Lit 0)
                    (BinOp (Arith Add) (Fst (Pair (Lit 10) (Lit 1))) (Snd (Pair (Lit 1) (Lit 10))))


-- Recursive function to add a List
sumList :: Exp
sumList = Fix (Lam (TList TInt)
                (LCase  (proj 0)
                        (Lit 0)
                        (add (proj 1) (apply (proj 3) (proj 0)))))


result :: Exp -> Maybe Value
result = eval VUnit
