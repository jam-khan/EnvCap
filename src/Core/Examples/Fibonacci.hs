module Core.Examples.Fibonacci where
import Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import Core.Util  (apply, proj, sub, mult, add)
import Core.Semantics (evalB)

fib :: Exp
fib =   Fix (Lam TInt
                (If     (BinOp  (Comp Le)
                                (proj 0)
                                (Lit 1))
                        (proj 0)
                        (add    (apply (proj 1) (sub (proj 0) (Lit 1)))
                                (apply (proj 1) (sub (proj 0) (Lit 2))))))


result :: Integer -> Maybe Value
result n = evalB Unit (apply fib (Lit n))