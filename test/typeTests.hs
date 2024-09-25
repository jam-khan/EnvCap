
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
  
  describe "Typ" $ do
    it "should represent types correctly" $ do
      show TInt `shouldBe` "TInt"
      show TEmpty `shouldBe` "TEmpty"
    
    it "should compare equivalent types correctly" $ do
      TArrow TInt TInt `shouldBe` TArrow TInt TInt
      TRecord { label = "A", typeVal = TInt} `shouldBe` TRecord { label = "A", typeVal = TInt }
      TAnd TEmpty TInt `shouldBe` TAnd TEmpty TInt
    
    it "should compare not equivalent types correctly" $ do
      TInt `shouldNotBe` TEmpty
      TArrow TInt TInt `shouldNotBe` TArrow TInt TEmpty
      TAnd TInt TInt `shouldNotBe` TAnd TInt TEmpty
      TAnd (TAnd TEmpty TInt) TInt `shouldNotBe` TAnd TEmpty TInt
      TRecord { label = "B", typeVal = TInt} `shouldNotBe` TRecord { label = "A", typeVal = TInt }

