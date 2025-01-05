{-# LANGUAGE InstanceSigs #-}
module PBT.Generators where

import Test.QuickCheck
import Core.Syntax
    ( Exp(..),
      BinaryOp(..),
      Value(..), Typ(..), ArithOp(..), CompOp(..), LogicOp(..), BinaryOp(..))

instance Arbitrary ArithOp where
    arbitrary :: Gen ArithOp
    arbitrary = elements [Add, Sub, Mul, Div, Mod]

instance Arbitrary CompOp where
    arbitrary :: Gen CompOp
    arbitrary = elements [Eql, Neq, Lt, Le, Gt, Ge]

instance Arbitrary LogicOp where
    arbitrary :: Gen LogicOp
    arbitrary = elements [And, Or]

-- Arbitrary instances for Op
instance Arbitrary BinaryOp where
  arbitrary :: Gen BinaryOp
  arbitrary = elements [App, Box, Mrg]


-- Arbitrary instances for Exp
instance Arbitrary Exp where
  arbitrary :: Gen Exp
  arbitrary = oneof [return Ctx, return Unit,
                      Lit     <$> arbitrary,
                      BinOp   <$> arbitrary <*> arbitrary <*> arbitrary,
                      Lam     <$> arbitrary <*> arbitrary,
                      Proj    <$> arbitrary <*> arbitrary,
                      Clos    <$> arbitrary <*> arbitrary,
                      Rec     <$> arbitrary <*> arbitrary,
                      RProj   <$> arbitrary <*> arbitrary]


-- Arbitrary instances for Value
instance Arbitrary Value where
    arbitrary :: Gen Value
    arbitrary = oneof
        [ return VUnit
        , VInt <$> arbitrary
        , VClos <$> arbitrary <*> arbitrary   -- Closure with random Value, Typ, and Exp
        , VRcd <$> arbitrary <*> arbitrary                  -- Record with random String and Value
        , VMrg <$> arbitrary <*> arbitrary                  -- Merge two random Values
        ]
-- data CompareExp op = BinOp (ComOp op) Exp Exp
-- genCompareExp :: Gen Exp
-- genCompareExp = oneof [
--         BinOp 
--     ]

-- genTUnit :: Gen Typ
genTUnit :: Gen Typ
genTUnit = return TUnit

genTInt :: Gen Typ
genTInt = return TInt

genTBool :: Gen Typ
genTBool = return TBool

genTArrow :: Gen Typ
genTArrow = TArrow <$> arbitrary <*> arbitrary

genTAnd :: Gen Typ
genTAnd = TAnd <$> arbitrary <*> arbitrary

genTRecord :: Gen Typ
genTRecord = TRecord <$> arbitrary <*> arbitrary

genType :: Gen Typ
genType = oneof
        [
            genTUnit,
            genTInt,
            genTBool,
            genTArrow,
            genTAnd,
            genTRecord
        ]


-- Arbitrary instances for Typ
instance Arbitrary Typ where
  arbitrary :: Gen Typ
  arbitrary = genType

genValue :: Gen Value
genValue = oneof
        [ return VUnit
        , VInt <$> arbitrary
        , VClos <$> arbitrary <*> arbitrary   -- Closure with random Value, Typ, and Exp
        , VRcd <$> arbitrary <*> arbitrary                  -- Record with random String and Value
        , VMrg <$> arbitrary <*> arbitrary                  -- Merge two random Values
        ]

-- This generates random expressions for testing
genExp :: Gen Exp
genExp = oneof
    [ Lit <$> arbitrary
    , return Unit
    , Ctx <$ return ()
    , Lam TInt <$> genExp
    , BinOp App <$> genExp <*> genExp
    , BinOp Mrg <$> genExp <*> genExp
    -- , RProj <$> (Rec <$> arbitrary <*> genExp) <*> arbitrary
    ]