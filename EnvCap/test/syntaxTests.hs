
-- Spec.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import LambdaE.Types
import LambdaE.Syntax
import LambdaE.Eval
import LambdaE.Check

import Control.Exception
import LambdaE.Types (Typ(TArrow, TRecord))
import LambdaE.Syntax (Value(VUnit))

main :: IO ()
main = hspec $ do
  describe "Op" $ do
    it "should show correct string representation" $ do
      show App `shouldBe` "App"
      show Box `shouldBe` "Box"
      show Mrg `shouldBe` "Mrg"

  describe "Expr" $ do
    it "should construct and compare expressions" $ do
      BinOp App (Lit 1) (Lit 2) 
        `shouldBe` BinOp App (Lit 1) (Lit 2)
      Unit `shouldBe` Unit
      Lam TInt (Lit 1) `shouldBe` Lam TInt (Lit 1)
     
    it "should not be equal to different expressions" $ do
      Lit 1 `shouldNotBe` Lit 2
      Proj Ctx 1 `shouldNotBe` Proj (Lit 1) 1
      Clos Ctx TInt Unit `shouldNotBe` Clos Ctx TInt (Lit 1)

    
  describe "Value" $ do
    it "should represent integer values correctly" $ do
      VInt (Lit 1) `shouldSatisfy` isVInt
      VInt Unit `shouldNotSatisfy` isVInt

    it "should represent unit values correctly" $ do
      VUnit Unit `shouldSatisfy` isVUnit
      VUnit (Lit 1) `shouldNotSatisfy` isVUnit
    
    it "should represent correct values syntax" $ do
      VUnit (Lit 1)            `shouldNotSatisfy` isValue
      VInt Unit                `shouldNotSatisfy` isValue 
      VInt Ctx                 `shouldNotSatisfy` isValue
      VUnit Unit               `shouldSatisfy` isValue
      VInt (Lit 10)            `shouldSatisfy` isValue
      
          