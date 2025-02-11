module Core.Recursion where
import ENVCAP.Syntax (BinaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), TypC (..))
import ENVCAP.Core.Util  (apply, proj, sub, mult, add)
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.TypeChecker ( infer, TypeError )

factorial :: Exp
factorial =     Fix (TyCArrow TyCInt TyCInt)
                        (Lam TyCInt
                        (If     (BinOp (Comp Le)
                                        (proj 0)
                                        (Lit 0))
                                (Lit 1)
                                (mult   (proj 0)
                                        (apply (proj 1) (sub (proj 0) (Lit 1))))))

fib :: Exp
fib =   Fix (TyCArrow TyCInt TyCInt)
                (Lam TyCInt
                (If     (BinOp  (Comp Le)
                                (proj 0)
                                (Lit 1))
                        (proj 0)
                        (add    (apply (proj 1) (sub (proj 0) (Lit 1)))
                                (apply (proj 1) (sub (proj 0) (Lit 2))))))

simpleList :: Exp
simpleList = Cons (Lit 1) (Cons (Lit 2) (Cons (Lit 3) (Nil TyCInt)))

simpleLCase :: Exp
simpleLCase = LCase
                (Cons (Lit 1) (Cons (Lit 2) (Nil TyCInt)))
                (proj 0)
                (proj 1)

complexLCase :: Exp
complexLCase    = LCase
                    (Cons (Lit 1) (Cons (Lit 2) (Nil TyCInt)))
                    (Lit 0)
                    (BinOp (Arith Add) (Fst (Pair (Lit 10) (Lit 1))) (Snd (Pair (Lit 1) (Lit 10))))

sumList :: Exp
sumList = Fix (TyCArrow (TyCList TyCInt) TyCInt) 
                (Lam (TyCList TyCInt)
                        (LCase  (proj 0)
                                (Lit 0)
                                (add (proj 1) (apply (proj 3) (proj 0)))))

resultSum :: Exp -> Maybe Value
resultSum e = eval VUnit (apply sumList e)
 
result :: Integer -> Maybe Value
result n = eval VUnit (apply factorial (Lit n))

resultT :: Exp -> Either ENVCAP.Core.TypeChecker.TypeError TypC
resultT = infer TyCUnit