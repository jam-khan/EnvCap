-- typeSystemTests.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec 
import Core.TypeChecker
    ( lookupt,
      isLabel,
      containment,
      rlookupt,
      infer )
import Core.Syntax
    ( Typ(..),
      Exp(..),
      BinaryOp(..) )


ctx :: Typ
ctx = TUnit


main :: IO ()
main = hspec $ do
  describe "lookupt" $ do
    it "should return Just for valid indices" $ do
      let typ = TAnd (TAnd TUnit (TRecord "x" TInt)) (TRecord "y" TUnit)
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
          typB = TRecord "z" TUnit
          typC = TAnd (TRecord "x" TInt) (TRecord "x" TUnit)
          typD = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)

      containment typA typB `shouldBe` False
      containment typA typC `shouldBe` False 
      containment typA typD `shouldBe` True

  describe "rlookupt" $ do
    it "should find types associated with labels in records" $ do
      let typ = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      rlookupt typ "x" `shouldBe` Just TInt
      rlookupt typ "y" `shouldBe` Just TUnit

    it "should return Nothing for non-existent labels" $ do
      let typ = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
      rlookupt typ "z" `shouldBe` Nothing

  describe "infer" $ do
    it "should infer types correctly from Expessions with literals and projections" $ do
      let ctx = TAnd (TRecord "x" TInt) (TRecord "y" TUnit)
          exp1 = Proj (BinOp Mrg Unit (Lit 42)) 0
          exp2 = Rec "z" (Lit 42)

      infer ctx exp1 `shouldBe` Just TInt
      infer ctx exp2 `shouldBe` Just (TRecord "z" TInt)

    it "should infer record projections correctly by label" $ do
      let ctx = TAnd (TRecord "z" TInt) (TRecord "y" TUnit)
          exp3 = RProj (Rec "z" (Lit 42)) "z"
      infer ctx exp3 `shouldBe` Just TInt 

    it "should return Nothing for invalid projections or unrecognized Expessions" $ do
      let ctx = TAnd (TRecord "a" TInt) (TRecord "b" TUnit)
          invalidExp = RProj Ctx ""
      infer ctx invalidExp `shouldBe` Nothing

    it "should return Nothing for mismatched types" $ do
        infer ctx (BinOp App (Lam TInt (Lit 42)) (Lam TUnit (Lit 1))) `shouldBe` Nothing
        infer ctx (BinOp App (Lam TInt (Lit 42)) (Lit 5)) `shouldBe` Just TInt
      
    it "should handle lambda Expessions correctly" $ do
        infer ctx (BinOp App 
                        (Lam TInt (Lam TInt Ctx)) 
                        (Lit 5)) `shouldBe` Just (TArrow TInt (TAnd (TAnd TUnit TInt) TInt))
    
    it "should handle Unit correctly" $ do
        infer ctx (BinOp App 
                        (Lam TInt (Lam TInt Unit)) 
                        (Lit 5)) `shouldBe` Just (TArrow TInt TUnit)
    
    it "should handle box correctly" $ do
        let e1 = BinOp Mrg Unit (Lit 1)
        let e2 = Proj Ctx 0
        infer ctx (BinOp Box e1 e2) `shouldBe` Just TInt

    it "should handle closures correctly" $ do
        let e1 = Clos (Lit 1) TInt Unit
        let e2 = Clos (Lit 1) TInt (Proj Ctx 2)
        let e3 = Clos (RProj Ctx "x") TInt (Proj Ctx 2)
        infer ctx e1 `shouldBe` Just (TArrow TInt TUnit)
        infer ctx e2 `shouldBe` Nothing
        infer ctx e3 `shouldBe` Nothing
    
    it "should return Nothing for invalid application" $ do
        infer ctx (BinOp App 
                    (Lam TInt (Lit 42)) 
                    (BinOp Mrg (Lit 1) (Lit 2))) 
            `shouldBe` Nothing