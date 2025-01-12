module Core.Fibonacci where
import ENVCAP.Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import ENVCAP.Core.Util  (apply, proj, sub, mult, add)
import ENVCAP.Core.Evaluator (eval)

fib :: Exp
fib =   Fix (Lam TInt
                (If     (BinOp  (Comp Le)
                                (proj 0)
                                (Lit 1))
                        (proj 0)
                        (add    (apply (proj 1) (sub (proj 0) (Lit 1)))
                                (apply (proj 1) (sub (proj 0) (Lit 2))))))


result :: Integer -> Maybe Value
result n = eval VUnit (apply fib (Lit n))