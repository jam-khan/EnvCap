module Core.Examples.Factorial where
import Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import Core.Util  (apply, proj, sub, mult, add)
import Core.Semantics (evalB)

factorial :: Exp
factorial =     Fix (Lam TInt
                                (If     (BinOp (Comp Le)
                                                (proj 0)
                                                (Lit 0))
                                        (Lit 1)
                                        (mult   (proj 0)
                                                (apply (proj 1) (sub (proj 0) (Lit 1))))))

result :: Integer -> Maybe Value
result n = evalB Unit (apply factorial (Lit n))
