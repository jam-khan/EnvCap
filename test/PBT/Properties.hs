
import Test.Hspec 
import ENVCAP.Core.Syntax
    ( Exp(..),
      BinaryOp(..),
      Value(..), Typ(..), ArithOp(..))
import ENVCAP.Core.Evaluator (eval)
import ENVCAP.Core.Util (lookupv, rlookupv)
import PBT.Util ( getValueTyp )
import ENVCAP.Core.TypeChecker (infer)
import Test.QuickCheck
    ( discard,
      counterexample,
      quickCheckWith,
      stdArgs,
      Property,
      Testable(property),
      Args(maxSuccess),
      (===), forAll )
import Data.Maybe (isNothing)


{--
  Check if the term, type, context and environment is well-formed before checking a property!
--}


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
main = hspec $ do
  
  describe "Property-Based Testing" $ do
    -- it "Every value must have a type (PROPERTY)" $ do
    --   quickCheckWith stdArgs { maxSuccess = 1000 } prop_values
    
    it "Preservation Property must be satisfied" $ do
      quickCheckWith stdArgs { maxSuccess = 1000 } prop_preservation

    it "Progress Property must be satisfied" $ do
      quickCheckWith stdArgs { maxSuccess = 1000 } prop_progress
        