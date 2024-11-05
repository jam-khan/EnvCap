module PBT.Generators where

import PBT.Arbitrary

import Test.QuickCheck
import Core.Syntax
    ( Exp(Lit, Proj, RProj, Rec, Lam, Unit, Ctx, BinOp, Clos),
      BinaryOp(..),
      Value(VMrg, VRcd, VUnit, VInt, VClos), Typ(..), ArithOp(..))

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