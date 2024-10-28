{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Test.Hspec ( hspec, describe, it, shouldBe, shouldSatisfy )
import Core.Syntax
    ( Exp(Lit, Proj, RProj, Rec, Lam, Unit, Ctx, BinOp, Clos),
      BinaryOp(..),
      Value(VMrg, VRcd, VUnit, VInt, VClos), Typ(..) )
import Core.Semantics ( evalB, evalBig, lookupv, rlookupv )
import Data.Maybe (isJust, isNothing, fromJust)

import Test.QuickCheck

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


genValue :: Gen Value
genValue = oneof 
        [ return VUnit
        , VInt <$> arbitrary
        , VClos <$> arbitrary <*> arbitrary <*> arbitrary   -- Closure with random Value, Typ, and Exp
        , VRcd <$> arbitrary <*> arbitrary                  -- Record with random String and Value
        , VMrg <$> arbitrary <*> arbitrary                  -- Merge two random Values
        ]

genExp :: Gen Exp
genExp = oneof
    [ Lit <$> arbitrary
    , return Unit
    , Ctx <$ return ()
    , Lam TInt <$> genExp
    , BinOp App <$> genExp <*> genExp
    , BinOp Mrg <$> genExp <*> genExp
    , RProj <$> (Rec <$> arbitrary <*> genExp) <*> arbitrary
    ]

prop_lookupvMerged :: Property
prop_lookupvMerged = forAll genValue $ \value ->
    let mergedValue = VMrg value (VInt 0)
    in lookupv mergedValue 0 === Just (VInt 0)

main :: IO ()
main = hspec $ do

    describe "lookupv" $ do
        it "should find the correct value in a merged value" $ do
            let mergedValue = VMrg (VMrg (VMrg VUnit (VInt 1)) (VInt 2)) (VInt 3)
            lookupv mergedValue 0 `shouldBe` Just (VInt 3)
            lookupv mergedValue 1 `shouldBe` Just (VInt 2)
            lookupv mergedValue 2 `shouldBe` Just (VInt 1)
            lookupv mergedValue 3 `shouldBe` Nothing
    
    -- Property-based testing
    describe "Property-Based Tests" $ do
        it "should correctly lookup in merged values" $
            property prop_lookupvMerged


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

    describe "evalBig" $ do
        it "should evaluate a literal Expession correctly" $ do
            evalB Unit (Lit 5) `shouldBe` Just (VInt 5)

        it "should return context correctly" $ do
            evalB Unit Ctx `shouldBe` Just VUnit -- Assuming Ctx returns the current environment

        it "should evaluate Unit Expession correctly" $ do
            evalB Unit Unit `shouldBe` Just VUnit

        it "should evaluate application of a lambda Expession correctly" $ do
            let lambdaExp = Lam TInt Ctx
            evalB Unit (BinOp App lambdaExp (Lit 5)) `shouldSatisfy` isJust
    
        it "should evaluate application of a box correctly" $ do
            let lambdaExp = BinOp Box (BinOp Mrg Unit (Lit 100)) (Proj Ctx 0)
            evalB Unit lambdaExp `shouldBe` Just (VInt 100)

        it "should evaluate a closure correctly" $ do
            let closureExp = Lam TInt Unit
            evalBig VUnit closureExp `shouldSatisfy` isJust

        it "should handle projections correctly" $ do
            let exp = RProj (Rec "x" (Lit 100)) "x"
            evalBig VUnit exp `shouldBe` Just (VInt 100)

        it "should handle recursive definitions correctly" $ do
            let recExp = Rec "fact"
                            (Lam TInt 
                            (BinOp App 
                                (BinOp App 
                                Ctx 
                                (Lit 0)) 
                                Unit))
            evalBig VUnit recExp `shouldSatisfy` isJust

        it "should return Nothing for undefined Expessions" $ do
            evalBig VUnit Ctx `shouldSatisfy` isJust -- Assuming Ctx should return an environment
    
        it "should return closures propertly" $ do
            evalB Unit (Clos Ctx TUnit (Lit 100)) `shouldBe` Just (VClos VUnit TUnit (Lit 100))

    it "should merge two values correctly" $ do
        let env = VUnit
            exp1 = VInt 1
            exp2 = VInt 2
            mergedExp = BinOp Mrg (Lit 1) (Lit 2)
    
        let v1 = evalBig env (Lit 1)
        let v2 = evalBig (VMrg env (fromJust v1)) (Lit 2)

        evalBig env mergedExp `shouldBe` Just (VMrg (fromJust v1) (fromJust v2))