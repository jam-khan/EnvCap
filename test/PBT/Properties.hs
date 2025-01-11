module PBT.Properties where

import Test.QuickCheck ( (===), forAll, Property ) 
import ENVCAP.Core.Syntax
    ( Exp(Lit, Proj, RProj, Rec, Lam, Unit, Ctx, BinOp, Clos),
      BinaryOp(..),
      Value(VMrg, VRcd, VUnit, VInt, VClos), Typ(..), ArithOp(..), isValue)
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.Util (lookupv, rlookupv )


-- Property isValue
prop_isValue :: Value -> Bool
prop_isValue v = isValue v == case v of
    VUnit         -> True
    VInt _        -> True
    VClos v' _  -> isValue v'
    VRcd _ v'     -> isValue v'
    VMrg v1 v2    -> isValue v1 && isValue v2