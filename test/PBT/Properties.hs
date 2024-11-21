module PBT.Properties where

import Test.QuickCheck
import PBT.Generators (genValue)
import Core.Syntax
    ( Exp(Lit, Proj, RProj, Rec, Lam, Unit, Ctx, BinOp, Clos),
      BinaryOp(..),
      Value(VMrg, VRcd, VUnit, VInt, VClos), Typ(..), ArithOp(..), isValue)
import Core.Semantics ( evalB, evalBig, lookupv, rlookupv )

prop_lookupvMerged :: Property
prop_lookupvMerged = forAll genValue $ \value ->
    let mergedValue = VMrg value (VInt 0)
    in lookupv mergedValue 0 === Just (VInt 0)


-- Property isValue
prop_isValue :: Value -> Bool
prop_isValue v = isValue v == case v of
    VUnit         -> True
    VInt _        -> True
    VClos v' _  -> isValue v'
    VRcd _ v'     -> isValue v'
    VMrg v1 v2    -> isValue v1 && isValue v2