{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Test.Hspec ( hspec, describe, it, shouldBe, shouldSatisfy )
import Core.Syntax
    ( Exp(..),
      BinaryOp(..),
      Value(..), Typ(..), ArithOp(..))
import Core.Evaluator (eval)
import Core.Util(lookupv, rlookupv )
import Data.Maybe (isJust, isNothing, fromJust)
import Test.QuickCheck


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

    describe "eval" $ do
        it "should evaluate a literal Expession correctly" $ do
            eval VUnit (Lit 5) `shouldBe` Just (VInt 5)

        it "should return context correctly" $ do
            eval VUnit Ctx `shouldBe` Just VUnit -- Assuming Ctx returns the current environment

        it "should evaluate Unit Expession correctly" $ do
            eval VUnit Unit `shouldBe` Just VUnit

        it "should evaluate application of a lambda Expession correctly" $ do
            let lambdaExp = Lam TInt Ctx
            eval VUnit (App lambdaExp (Lit 5)) `shouldSatisfy` isJust
    
        it "should evaluate application of a box correctly" $ do
            let lambdaExp = Box (Mrg Unit (Lit 100)) (Proj Ctx 0)
            eval VUnit lambdaExp `shouldBe` Just (VInt 100)

        it "should evaluate a closure correctly" $ do
            let closureExp = Lam TInt Unit
            eval VUnit closureExp `shouldSatisfy` isJust

        it "should handle projections correctly" $ do
            let exp = RProj (Rec "x" (Lit 100)) "x"
            eval VUnit exp `shouldBe` Just (VInt 100)

        it "should return Nothing for undefined Expessions" $ do
            eval VUnit Ctx `shouldSatisfy` isJust -- Assuming Ctx should return an environment
    
        it "should return closures propertly" $ do
            eval VUnit (Clos Ctx (Lam TUnit (Lit 100))) `shouldBe` Just (VClos VUnit (Lam TUnit (Lit 100)))

    it "should merge two values correctly" $ do
        let env = VUnit
            exp1 = VInt 1
            exp2 = VInt 2
            mergedExp = Mrg (Lit 1) (Lit 2)
    
        let v1 = eval env (Lit 1)
        let v2 = eval (VMrg env (fromJust v1)) (Lit 2)

        eval env mergedExp `shouldBe` Just (VMrg (fromJust v1) (fromJust v2))