module Core.Examples.Arithmetic where
import Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import Core.Util  (apply, proj, sub, mult, add)
import Core.Semantics (evalB)

sumN :: Exp
sumN = Fix (Lam TInt 
                (If (BinOp (Comp Eql)
                            (proj 0)
                            (Lit 0))
                    (Lit 0)
                    (add (apply (proj 1) (sub (proj 0) (Lit 1)))
                         (proj 0))))


add1 :: Exp
add1 =  Lam TInt 
            (Lam TInt
                (add 
                    (proj 1) (proj 0)))

result1 :: Integer -> Integer -> Maybe Value
result1 n1 n2 = evalB Unit (apply (apply add1 (Lit n1)) (Lit n2))

result :: Integer -> Maybe Value
result n = evalB Unit (apply sumN (Lit n))