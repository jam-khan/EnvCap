module Core.Sort where
import ENVCAP.Core.Syntax (BinaryOp(..), UnaryOp(..), Exp(..), Value(..), ArithOp(..), CompOp(..), LogicOp(..), Typ (..))
import ENVCAP.Core.Util  (apply, proj, sub, mult, add)
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.TypeChecker (infer)


sort :: Exp
sort = Fix 
        (Lam (TList TInt)
        (LCase  (proj 0)
                (Nil TInt)
                (App (Fix (Lam (TPair (TList TInt) (TList TInt))
                (LCase  (Fst (proj 0))
                        (Snd (proj 0))
                        (Cons   (proj 1)
                                (App (proj 3) (Pair (proj 0) (Snd (proj 2))))))))
                                (Pair   (App (proj 3) (App (App 
                                                        (Lam (TArrow (TPair TInt (TList TInt)) (TList TInt))
                                                        (Lam    (TPair TInt (TList TInt))
                                                                (App (proj 1) (proj 0)))) 
                                                                (Fix (Lam (TPair TInt (TList TInt))
                                                                        (LCase (Snd (proj 0))
                                                                                (Nil TInt)
                                                                                (If (BinOp (Comp Le) (Fst (proj 2)) (proj 1))
                                                                                        (Cons   (proj 1)
                                                                                                (App (proj 3)
                                                                                                (Pair (Fst (proj 2)) (proj 0))))
                                                                                        (App    (proj 3)
                                                                                                (Pair (Fst (proj 2)) (proj 0))))))))  (Pair (proj 1) (proj 0))))
                                (Cons   (proj 1)
                                        (App    (proj 3)
                                                (App (App (Lam (TArrow (TPair TInt (TList TInt)) (TList TInt))
                                                                (Lam    (TPair TInt (TList TInt))
                                                                        (App (proj 1) (proj 0)))) 
                                                                                (Fix (Lam (TPair TInt (TList TInt))
                                                                                        (LCase (Snd (proj 0))
                                                                                                (Nil TInt)
                                                                                                (If (BinOp (Comp Gt) (Fst (proj 2)) (proj 1))
                                                                                                        (Cons   (proj 1)
                                                                                                                (App (proj 3)
                                                                                                                (Pair (Fst (proj 2)) (proj 0))))
                                                                                                        (App (proj 3)
                                                                                                                (Pair (Fst (proj 2)) (proj 0))))))))    (Pair (proj 1) (proj 0)))))))))


temp' :: Exp
temp' = App sort (Cons (Lit (-10)) (Cons (Lit 100) (Cons (Lit 1) (Cons (Lit 5) (Cons (Lit 10) (Nil TInt))))))

result :: Maybe Value
result = eval VUnit temp'

typeCheck :: Maybe Typ
typeCheck = infer TUnit temp'


{--
        module Sorting where
                greaterthan(xs : [Int], x : Int) :: [Int] -> Int -> [Int] {
                        letrec f ([Int] & Int) : [Int] = 
                                
                        case xs of
                                []      -> []
                                (y:ys)  -> if   (x > y)
                                                y :: greaterthan(ys, x)
                                                greatherthan(ys, x);
                        return res;
                }

                lessthanEqual(xs : [Int], x : Int) {
                        res : [Int]     = 
                                case xs of
                                        []      -> []
                                        (y:ys)  -> if   (x <= y)
                                                        y :: lessthanEqual(ys, x)
                                                        lessthanEqual(ys, x);
                        return res;
                }
                sort(xs : [Int]) = case xs of
                                        []      -> []
                                        (y:ys)  -> lessthanEqual(ys,y) ++ y ++ greaterthan(ys,y);
                                                
--}
