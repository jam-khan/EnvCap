
-- Spec.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import LambdaE.Types
import LambdaE.Syntax
import LambdaE.Eval
import LambdaE.Check

import Control.Exception
import LambdaE.Types (Typ(TArrow, TRecord))

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
      let value = VInt
      value `shouldSatisfy` isVInt

    it "should represent unit values correctly" $ do
      let value = VUnit
      value `shouldSatisfy` isVUnit

  describe "Typ" $ do
    it "should represent types correctly" $ do
      show TInt `shouldBe` "TInt"
      show TEmpty `shouldBe` "TEmpty"
      TInt `shouldNotBe` TEmpty
      TArrow TInt TInt `shouldBe` TArrow TInt TInt
      TArrow TInt TInt `shouldNotBe` TArrow TInt TEmpty
      TAnd TInt TInt `shouldNotBe` TAnd TInt TEmpty
      TAnd (TAnd TEmpty TInt) TInt `shouldNotBe` TAnd TEmpty TInt
      TRecord { label = "A", typeVal = TInt} `shouldBe` TRecord { label = "A", typeVal = TInt }
      
-- Helper functions to check Value constructors.
isVInt :: Value -> Bool
isVInt VInt = True
isVInt _    = False

isVUnit :: Value -> Bool
isVUnit VUnit = True
isVUnit _     = False
