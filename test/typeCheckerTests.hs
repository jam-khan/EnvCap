-- typeSystemTests.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec 
import LambdaE.TypeChecker
    ( lookupt,
      isLabel,
      containment,
      rlookupt,
      infer )
import LambdaE.Syntax
    ( Typ(..),
      Expr(..) )

main :: IO ()
main = hspec $ do
  describe "lookupt" $ do
    it "should return Just for valid indices" $ do
      let typ = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      lookupt typ 0 `shouldBe` Just (TRecord "y" TUnit)
      lookupt typ 1 `shouldBe` Just (TRecord "x" TInt)

    it "should return Nothing for out-of-bounds indices" $ do
      let typ = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      lookupt typ 2 `shouldBe` Nothing

  describe "isLabel" $ do
    it "should return True for existing labels" $ do
      let typ = TRecord "x" TInt
      isLabel "x" typ `shouldBe` True

    it "should return False for non-existing labels" $ do
      let typ = TRecord "x" TInt
      isLabel "y" typ `shouldBe` False

    it "should return True for labels in compound types" $ do
      let compoundTyp = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      isLabel "y" compoundTyp `shouldBe` True

  describe "containment" $ do
    it "should confirm identical records are contained" $ do
      let typA = TRecord "x" TInt
          typB = TRecord "x" TInt
      containment typA typB `shouldBe` True

    it "should confirm containment in compound types without duplicates" $ do
      let typA = TRecord "x" TInt
          typC = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      containment typA typC `shouldBe` True

    it "should return False for different labels or types" $ do
      let typA = TRecord "x" TInt
          typB = TRecord "z" TUnit -- Different label
          typC = TAnd (TRecord "x" TInt) (TRecord "y" TUnit) -- Different type in a compound type

      containment typA typB `shouldBe` False 
      containment typA typC `shouldBe` False 

  describe "rlookupt" $ do
    it "should find types associated with labels in records" $ do
      let typ = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      rlookupt typ "x" `shouldBe` Just TInt
      rlookupt typ "y" `shouldBe` Just TUnit

    it "should return Nothing for non-existent labels" $ do
      let typ = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      rlookupt typ "z" `shouldBe` Nothing

  describe "infer" $ do
    it "should infer types correctly from expressions with literals and projections" $ do
      let ctx = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
          expr1 = Proj (Lit 42) 0 
          expr2 = Rec "z" (Lit 42)

      infer ctx expr1 `shouldBe` Just TInt 
      infer ctx expr2 `shouldBe` Just (TRecord "z" TInt)

    it "should infer record projections correctly by label" $ do
      let ctx = TAnd (TRecord "z" TInt) (TRecord "y" TUnit)
          expr3 = RProj (Rec "z" (Lit 42)) "z"

      infer ctx expr3 `shouldBe` Just TInt 

    it "should return Nothing for invalid projections or unrecognized expressions" $ do
        let ctx = TAnd (TRecord "a" TInt) (TRecord "b" TUnit)
            invalidExpr = RProj Ctx "" -- Invalid projection
        
        infer ctx invalidExpr `shouldBe` Nothing 