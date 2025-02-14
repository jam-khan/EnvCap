module Core.Recursion where
import ENVCAP.Syntax 
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.TypeChecker ( infer, TypeError )

proj :: Integer -> CoreTm
proj = Proj Ctx

add :: CoreTm -> CoreTm -> CoreTm
add = BinOp (Arith Add)

sub :: CoreTm -> CoreTm -> CoreTm
sub = BinOp (Arith Sub)

mult :: CoreTm -> CoreTm -> CoreTm
mult = BinOp (Arith Mul)

div :: CoreTm -> CoreTm -> CoreTm
div = BinOp (Arith Div)

mod :: CoreTm -> CoreTm -> CoreTm
mod = BinOp (Arith Mod)

apply :: CoreTm -> CoreTm -> CoreTm
apply = App

factorial :: CoreTm
factorial =     Fix (TyCArrow TyCInt TyCInt)
                        (Lam TyCInt
                        (If     (BinOp (Comp Le)
                                        (proj 0)
                                        (Lit 0))
                                (Lit 1)
                                (mult   (proj 0)
                                        (apply (proj 1) (sub (proj 0) (Lit 1))))))

fib :: CoreTm
fib =   Fix (TyCArrow TyCInt TyCInt)
                (Lam TyCInt
                (If     (BinOp  (Comp Le)
                                (proj 0)
                                (Lit 1))
                        (proj 0)
                        (add    (apply (proj 1) (sub (proj 0) (Lit 1)))
                                (apply (proj 1) (sub (proj 0) (Lit 2))))))

simpleList :: CoreTm
simpleList = Cons (Lit 1) (Cons (Lit 2) (Cons (Lit 3) (Nil TyCInt)))

simpleLCase :: CoreTm
simpleLCase = LCase
                (Cons (Lit 1) (Cons (Lit 2) (Nil TyCInt)))
                (proj 0)
                (proj 1)

complexLCase :: CoreTm
complexLCase    = LCase
                    (Cons (Lit 1) (Cons (Lit 2) (Nil TyCInt)))
                    (Lit 0)
                    (BinOp (Arith Add) (Fst (Pair (Lit 10) (Lit 1))) (Snd (Pair (Lit 1) (Lit 10))))

sumList :: CoreTm
sumList = Fix (TyCArrow (TyCList TyCInt) TyCInt) 
                (Lam (TyCList TyCInt)
                        (LCase  (proj 0)
                                (Lit 0)
                                (add (proj 1) (apply (proj 3) (proj 0)))))

resultSum :: CoreTm -> Maybe Value
resultSum e = eval VUnit (apply sumList e)
 
result :: Integer -> Maybe Value
result n = eval VUnit (apply factorial (Lit n))

resultT :: CoreTm -> Either ENVCAP.Core.TypeChecker.TypeError CoreTyp
resultT = infer TyCUnit