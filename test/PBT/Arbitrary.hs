{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module PBT.Arbitrary where

import Test.QuickCheck
import Core.Syntax
    ( Exp(Lit, Proj, RProj, Rec, Lam, Unit, Ctx, BinOp, Clos),
      BinaryOp(..),
      Value(VMrg, VRcd, VUnit, VInt, VClos), Typ(..), ArithOp(..))

instance Arbitrary ArithOp where
    arbitrary :: Gen ArithOp
    arbitrary = elements [Add, Sub, Mul, Div, Mod]

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