module PBT.Util where

import ENVCAP.Core.Syntax
    ( Exp(..),
      BinaryOp(..),
      Value(..), Typ(..), ArithOp(..))
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.Util (lookupv, rlookupv)
import ENVCAP.Core.TypeChecker (infer)


getValueTyp :: Typ -> Value -> Maybe Typ
getValueTyp env VUnit           = Just TUnit
getValueTyp env (VInt _)        = Just TInt
getValueTyp env (VBool _)       = Just TBool
getValueTyp env (VString _)     = Just TString
getValueTyp env (VNil t)        = Just (TList t)
getValueTyp ev (VClos v (Lam tA e)) 
                                = case getValueTyp ev v of
                                        Just env    -> (case infer (TAnd env tA) e of
                                                            Right tB -> Just (TArrow tA tB)
                                                            _       -> Nothing)
                                        _           -> Nothing
getValueTyp _ (VClos v _)       = Just TUnit
getValueTyp env (VRcd s val)    = case getValueTyp env val of
                                    Just t1 -> Just (TRecord s t1)
                                    _       -> Nothing
    
getValueTyp env (VMrg v1 v2)    = case getValueTyp env v1 of
                                    Just tA     -> TAnd <$> Just tA <*> getValueTyp (TAnd env tA) v2
                                    _           -> Nothing
getValueTyp env (VPair v1 v2)   = case getValueTyp env v1 of
                                        Just t1  -> case getValueTyp env v2 of
                                                        Just t2     -> Just (TPair t1 t2)
                                                        _           -> Nothing
                                        _        -> Nothing
getValueTyp env (VInL t2 v1)    = case getValueTyp env v1 of
                                        Just t1 -> Just (TSum t1 t2)
                                        _       -> Nothing
getValueTyp env (VInR t1 v2)    = case getValueTyp env v2 of
                                        Just t2 -> Just (TSum t1 t2)
                                        _       -> Nothing
getValueTyp env (VCons v1 v2)   = if (TList <$> getValueTyp env v1) == getValueTyp env v2
                                    then TList <$> getValueTyp env v1
                                    else Nothing     