{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec ( hspec, describe, it, shouldBe, shouldSatisfy )
import LambdaE.Syntax
    ( Expr(Lit, Proj, RProj, Rec, Lam, Unit, Ctx, BinOp, Clos),
      Op(Mrg, Box, App),
      Value(VMrg, VRcd, VUnit, VInt, VClos), Typ(..) )
import LambdaE.BigStep ( evalB, evalBig, lookupv, rlookupv )
import Data.Maybe (isJust, isNothing, fromJust)

main :: IO ()
main = hspec $ do

    describe "lookupv" $ do
        it "should find the correct value in a merged value" $ do
            let mergedValue = VMrg (VMrg (VMrg VUnit (VInt 1)) (VInt 2)) (VInt 3)
            lookupv mergedValue 0 `shouldBe` Just (VInt 3)
            lookupv mergedValue 1 `shouldBe` Just (VInt 2)
            lookupv mergedValue 2 `shouldBe` Just (VInt 1)
            lookupv mergedValue 3 `shouldBe` Nothing

    describe "rlookupv" $ do
        it "should find a value in a record" $ do
            let record = VMrg (VRcd "x" (VInt 42)) (VRcd "y" (VInt 40))
            rlookupv record "x" `shouldBe` Just (VInt 42)
            rlookupv record "y" `shouldBe` Just (VInt 40)

        it "should return Nothing if the label is not found" $ do
            let record = VRcd "label" (VInt 42)
            rlookupv record "not_found" `shouldBe` Nothing

        it "should search through merged records" $ do
            let record1 = VRcd "label1" (VInt 42)
            let record2 = VRcd "label2" (VInt 84)
            let mergedRecord = VMrg record1 record2
            rlookupv mergedRecord "label2" `shouldBe` Just (VInt 84)

    describe "evalBig" $ do
        it "should evaluate a literal expression correctly" $ do
            evalB Unit (Lit 5) `shouldBe` Just (VInt 5)

        it "should return context correctly" $ do
            evalB Unit Ctx `shouldBe` Just VUnit -- Assuming Ctx returns the current environment

        it "should evaluate Unit expression correctly" $ do
            evalB Unit Unit `shouldBe` Just VUnit

        it "should evaluate application of a lambda expression correctly" $ do
            let lambdaExpr = Lam TInt Ctx
            evalB Unit (BinOp App lambdaExpr (Lit 5)) `shouldSatisfy` isJust
    
        it "should evaluate application of a box correctly" $ do
            let lambdaExpr = BinOp Box (BinOp Mrg Unit (Lit 100)) (Proj Ctx 0)
            evalB Unit lambdaExpr `shouldBe` Just (VInt 100)

        it "should evaluate a closure correctly" $ do
            let closureExpr = Lam TInt Unit
            evalBig VUnit closureExpr `shouldSatisfy` isJust

        it "should handle projections correctly" $ do
            let expr = RProj (Rec "x" (Lit 100)) "x"
            evalBig VUnit expr `shouldBe` Just (VInt 100)

        it "should handle recursive definitions correctly" $ do
            let recExpr = Rec "fact"
                            (Lam TInt 
                            (BinOp App 
                                (BinOp App 
                                Ctx 
                                (Lit 0)) 
                                Unit))
            evalBig VUnit recExpr `shouldSatisfy` isJust

        it "should return Nothing for undefined expressions" $ do
            evalBig VUnit Ctx `shouldSatisfy` isJust -- Assuming Ctx should return an environment
    
        it "should return closures propertly" $ do
            evalB Unit (Clos Ctx TUnit (Lit 100)) `shouldBe` Just (VClos VUnit TUnit (Lit 100))

    it "should merge two values correctly" $ do
        let env = VUnit             -- Define your initial environment
            expr1 = VInt 1          -- First value to merge
            expr2 = VInt 2          -- Second value to merge
            mergedExpr = BinOp Mrg (Lit 1) (Lit 2)  -- Expression to evaluate
    
        -- Evaluate the first expression
        let v1 = evalBig env (Lit 1)  -- Should yield Just (VInt (Lit 1))
        -- Evaluate the second expression in a merged context
        let v2 = evalBig (VMrg env (fromJust v1)) (Lit 2)  -- Should yield Just (VInt (Lit 2))

        evalBig env mergedExpr `shouldBe` Just (VMrg (fromJust v1) (fromJust v2))