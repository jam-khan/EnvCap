{-# LANGUAGE InstanceSigs #-}
module PBT.Generators where

import Test.QuickCheck
    ( Arbitrary(arbitrary), Gen, elements, oneof )
import ENVCAP.Core.Syntax
    ( Exp(..),
      BinaryOp(..),
      Value(..), Typ(..), ArithOp(..), CompOp(..), LogicOp(..), BinaryOp(..))

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
