module Core.Testing.PBT where

import Core.TypeChecker (infer)
import Core.Syntax (Exp(..), Typ(..), Value(..), BinaryOp(..), UnaryOp(..))
import Core.Evaluator (eval)
import Test.QuickCheck
    ( discard,
      counterexample,
      quickCheckWith,
      stdArgs,
      Property,
      Testable(property),
      Args(maxSuccess) )
import Data.Maybe (isNothing)


getValueTyp :: Typ -> Value -> Maybe Typ
getValueTyp env VUnit           = Just TUnit
getValueTyp env (VInt _)        = Just TInt
getValueTyp env (VBool _)       = Just TBool
getValueTyp env (VString _)     = Just TString
getValueTyp env (VNil t)        = Just (TList t)
getValueTyp ev (VClos v (Lam tA e)) 
                                = case getValueTyp ev v of
                                        Just env    -> (case infer (TAnd env tA) e of
                                                            Just tB -> Just (TArrow tA tB)
                                                            _       -> Nothing)
                                        _       -> Nothing
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
                                        


-- Property: All values have types
prop_values :: Value -> Property
prop_values v = counterexample (show v) $
                case getValueTyp TUnit v of
                    Just typ    -> property True 
                    _           -> property False

-- Property: Type Preservation
prop_preservation :: Exp -> Property
prop_preservation t = counterexample (show t) $
                        case infer TUnit t of
                            Just ty ->
                                case eval VUnit t of
                                    Just v  -> property (getValueTyp TUnit v == Just ty)
                                    _       -> property False
                            _   -> discard

-- Property: Progress
prop_progress :: Exp -> Property
prop_progress t = counterexample (show t) $
    case infer TUnit t of
        Just ty -> case eval VUnit t of
                    Just v      -> property True
                    _           -> property False
        _       -> discard

main :: IO ()
main = do
        -- quickCheck prop_values
        quickCheckWith stdArgs { maxSuccess = 1000 } prop_preservation
        quickCheckWith stdArgs { maxSuccess = 1000 } prop_progress
        