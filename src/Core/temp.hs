module Main where

box :: Exp -> Exp -> Exp
box = BinOp Box

merge :: Exp -> Exp -> Exp
merge = BinOp Mrg

proj :: Int -> Exp
proj = Proj Ctx

add :: Exp -> Exp -> Exp
add = BinOp (Arith Add)

sub :: Exp -> Exp -> Exp
sub = BinOp (Arith Sub)

mult :: Exp -> Exp -> Exp
mult = BinOp (Arith Mul)

apply :: Exp -> Exp -> Exp
apply = BinOp App

-- Example 1: Fibonacci
fib :: Exp
fib =   Fix (Lam TInt
                (If     (BinOp  (Comp Le)
                                (proj 0)
                                (Lit 1))
                        (proj 0)
                        (add    (apply (Fix (proj 1)) (sub (proj 0) (Lit 1)))
                                (apply (Fix (proj 1)) (sub (proj 0) (Lit 2))))))

{--
Surface Language Constructs:

fib (n: Int) : Int = if n <= 1 then n else fib (n - 1) * fib (n - 2);
fib 14

===>
fib :: Exp
fib =   Fix (Lam TInt
                (If     (BinOp  (Comp Le)
                                (proj 0)
                                (Lit 1))
                        (proj 0)
                        (add    (apply (proj 1) (sub (proj 0) (Lit 1)))
                                (apply (proj 1) (sub (proj 0) (Lit 2))))))
--}

factorial :: Exp
factorial =     Fix
                (Lam    (TArrow TInt TInt)
                        (Lam TInt
                                (If     (BinOp (Comp Le)
                                                (proj 0)
                                                (Lit 0))
                                        (Lit 1)
                                        (mult   (proj 0)
                                                (apply (Fix (proj 1)) (sub (proj 0) (Lit 1)))))))

result1 :: Maybe Value
result1 = evalBig VUnit (apply fib (Lit 9))

result2 :: Int -> Maybe Value
result2 n = evalBig VUnit (apply factorial (Lit n))


-- With Fix
{--

        v |- (f f) => <v1, lam A. e>    v |- e => v1    v, v1 |- Fix f e => val
        --------------------------------------------------------------- (BStep-FIX)
                        v |- ((Fix f) e)        =>  val
--}
