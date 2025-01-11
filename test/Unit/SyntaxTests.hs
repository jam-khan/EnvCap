{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Test.Hspec 
import ENVCAP.Core.Syntax
    ( isValue,
      Exp(..),
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

  describe "Value" $ do
    it "should represent correct values syntax" $ do
      VUnit     `shouldSatisfy` isValue
      VInt 10   `shouldSatisfy` isValue
      
    it "should recognize closures as values" $ do
      let closure = VClos (VInt 1) (Lam TInt (Lit 1))
      closure `shouldSatisfy` isValue

    it "should recognize records as values" $ do
      let record = VRcd "label" (VInt 5)
      record `shouldSatisfy` isValue

    it "should recognize merged values as values" $ do
      let merged = VMrg (VInt 1) (VInt 2)
      merged `shouldSatisfy` isValue

    it "should not recognize non-values" $ do
      let nonValue = VClos VUnit (Lam TInt (Lit 100))
      nonValue `shouldSatisfy` isValue
  
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