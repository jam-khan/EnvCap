{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Test.Hspec 
import ENVCAP.Core.Syntax
    ( Exp(..),
      BinaryOp(..),
      Value(..),
      Typ(..))
import Control.Exception ()
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  
  describe "Exp" $ do
    it "should construct and compare Expessions" $ do
      App (Lit 1) (Lit 2) 
                                `shouldBe`  App (Lit 1) (Lit 2)
      Unit                      `shouldBe`  Unit
      Lam TInt (Lit 1)          `shouldBe`  Lam TInt (Lit 1)
     
    it "should not be equal to different Expessions" $ do
      Lit 1               `shouldNotBe`   Lit 2
      Proj Ctx 1          `shouldNotBe`   Proj (Lit 1) 1
      Clos Ctx (Lam TInt Unit)
                          `shouldNotBe`   Clos Ctx (Lam TInt (Lit 1))
  
  describe "Typ" $ do
    it "should represent types correctly" $ do
      show TInt     `shouldBe`  "TInt"
      show TUnit    `shouldBe`  "TUnit"
    
    it "should compare equivalent types correctly" $ do
      TArrow TInt TInt    `shouldBe`  TArrow TInt TInt
      TRecord "A" TInt    `shouldBe`  TRecord "A" TInt
      TRecord "X" TUnit   `shouldBe`  TRecord "X" TUnit
      TAnd TUnit TInt     `shouldBe`  TAnd TUnit TInt
    
    it "should compare not equivalent types correctly" $ do
      TInt                        `shouldNotBe`     TUnit
      TArrow TInt TInt            `shouldNotBe`     TArrow TInt TUnit
      TAnd TInt TInt              `shouldNotBe`     TAnd TInt TUnit
      TAnd (TAnd TUnit TInt) TInt `shouldNotBe`     TAnd TUnit TInt
      TRecord "B" TInt            `shouldNotBe`     TRecord "A" TInt