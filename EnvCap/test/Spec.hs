
-- Spec.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import LambdaE.Types
import LambdaE.Syntax
import LambdaE.Eval
import LambdaE.Check

main :: IO ()
main = hspec $ do
  describe "Op" $ do
    it "should show correct string representation" $ do
      show App `shouldBe` "App"
      show Box `shouldBe` "Box"
      show Mrg `shouldBe` "Mrg"

  describe "Expr" $ do
    it "should construct and compare expressions" $ do
      let expr1 = BinOp App (Lit 1) (Lit 2)
      let expr2 = BinOp App (Lit 1) (Lit 2)
      expr1 `shouldBe` expr2

    it "should not be equal to different expressions" $ do
      let expr1 = Lit 1
      let expr2 = Lit 2
      expr1 `shouldNotBe` expr2

  describe "Value" $ do
    it "should represent integer values correctly" $ do
      let value = VInt
      value `shouldSatisfy` isVInt

    it "should represent unit values correctly" $ do
      let value = VUnit
      value `shouldSatisfy` isVUnit

-- Helper functions to check Value constructors.
isVInt :: Value -> Bool
isVInt VInt = True
isVInt _    = False

isVUnit :: Value -> Bool
isVUnit VUnit = True
isVUnit _     = False