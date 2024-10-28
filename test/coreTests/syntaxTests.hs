{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Test.Hspec 
import Core.Syntax
    ( isValue,
      Exp(..),
      BinaryOp(App, Box, Mrg),
      Value(VUnit, VRcd, VMrg, VInt, VClos),
      Typ(..))
import Control.Exception ()
import Test.QuickCheck


-- Revision Fact:
-- What is <$>?
-- ($) ::                (a -> b) ->   a ->   b
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
--        

-- Arbitrary instances for Op
instance Arbitrary BinaryOp where
  arbitrary :: Gen BinaryOp
  arbitrary = elements [App, Box, Mrg]

-- Arbitrary instances for Typ
instance Arbitrary Typ where
  arbitrary :: Gen Typ
  arbitrary = oneof [return TInt, return TUnit,
                      TArrow <$> arbitrary <*> arbitrary,
                      TAnd   <$> arbitrary <*> arbitrary,
                      TRecord <$> arbitrary <*> arbitrary]
-- Arbitrary instances for Value
instance Arbitrary Value where
    arbitrary :: Gen Value
    arbitrary = oneof 
        [ return VUnit
        , VInt <$> arbitrary
        , VClos <$> arbitrary <*> arbitrary <*> arbitrary   -- Closure with random Value, Typ, and Exp
        , VRcd <$> arbitrary <*> arbitrary                  -- Record with random String and Value
        , VMrg <$> arbitrary <*> arbitrary                  -- Merge two random Values
        ]
        
-- Arbitrary instances for Exp
instance Arbitrary Exp where
  arbitrary :: Gen Exp
  arbitrary = oneof [return Ctx, return Unit,
                      Lit     <$> arbitrary,
                      BinOp   <$> arbitrary <*> arbitrary <*> arbitrary,
                      Lam     <$> arbitrary <*> arbitrary,
                      Proj    <$> arbitrary <*> arbitrary,
                      Clos    <$> arbitrary <*> arbitrary <*> arbitrary,
                      Rec     <$> arbitrary <*> arbitrary,
                      RProj   <$> arbitrary <*> arbitrary]


-- Property isValue
prop_isValue :: Value -> Bool
prop_isValue v = isValue v == case v of
    VUnit         -> True
    VInt _        -> True
    VClos v' _ _  -> isValue v'
    VRcd _ v'     -> isValue v'
    VMrg v1 v2    -> isValue v1 && isValue v2



main :: IO ()
main = hspec $ do
  describe "Op" $ do
    it "should show correct string representation" $ do
      show App    `shouldBe`  "App"
      show Box    `shouldBe`  "Box"
      show Mrg    `shouldBe`  "Mrg"

  describe "Exp" $ do
    it "should construct and compare Expessions" $ do
      BinOp App (Lit 1) (Lit 2) 
                                `shouldBe`  BinOp App (Lit 1) (Lit 2)
      Unit                      `shouldBe`  Unit
      Lam TInt (Lit 1)          `shouldBe`  Lam TInt (Lit 1)
     
    it "should not be equal to different Expessions" $ do
      Lit 1               `shouldNotBe`   Lit 2
      Proj Ctx 1          `shouldNotBe`   Proj (Lit 1) 1
      Clos Ctx TInt Unit  `shouldNotBe`   Clos Ctx TInt (Lit 1)

  describe "Value" $ do
    it "should represent correct values syntax" $ do
      VUnit     `shouldSatisfy` isValue
      VInt 10   `shouldSatisfy` isValue
      quickCheck prop_isValue
      
    it "should recognize closures as values" $ do
      let closure = VClos (VInt 1) TInt (Lit 1)
      closure `shouldSatisfy` isValue

    it "should recognize records as values" $ do
      let record = VRcd "label" (VInt 5)
      record `shouldSatisfy` isValue

    it "should recognize merged values as values" $ do
      let merged = VMrg (VInt 1) (VInt 2)
      merged `shouldSatisfy` isValue

    it "should not recognize non-values" $ do
      let nonValue = VClos VUnit TInt (Lit 100)
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