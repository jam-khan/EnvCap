
module Spec where

import Test.Hspec
import System.FilePath
import Control.Monad
import ENVCAP.Interpreter
import ENVCAP.Syntax
import ENVCAP.Source.Errors

testcases :: [String]
testcases = [
    "examples/ADT/TyAliases.ep",
    "examples/ADT/Variants.ep",
    "examples/Basic/Arithmetic.ep",
    "examples/Basic/Conditional.ep",
    "examples/Capabilities/A.ep",
    "examples/Capabilities/B.ep",
    "examples/Environments/Environments.ep",
    "examples/Environments/Merges.ep",
    "examples/Environments/MergesTest.ep",
    "examples/Functions/Anonymous.ep",
    "examples/Functions/FirstClassFunction.ep",
    "examples/Functions/Functions.ep",
    "examples/Letbindings/Letbindings1.ep",
    "examples/Letbindings/Letbindings2.ep",
    "examples/Letbindings/Letbindings3.ep",
    "examples/Lists/Example1.ep",
    "examples/Modules/Modules1.ep",
    "examples/Modules/Modules2.ep",
    "examples/Recursion/CheckPrime.ep",
    "examples/Recursion/Factorial.ep",
    "examples/Recursion/Fibonacci.ep",
    "examples/SepComp/Module1.ep",
    "examples/SepComp/Module2.ep",
    "examples/SepComp/Module3.ep",
    "examples/SepComp1/Lib.ep",
    "examples/SepComp1/Main.ep",
    "examples/SepComp1/Utils.ep"
    ]

spec :: Spec
spec = do
  describe "Interpreter Unit Tests" $ do
    parseCodeTests
    typeAliasExpansionTests
    locallyNamelessTests
    desugarSourceTests
    elaborationTests
    evaluateTests
    errorConditionTests
  
  describe "Full Execution Tests" $ do
    mapM_ fullExecutionTest testcases

-- Helper for full execution tests
fullExecutionTest :: String -> SpecWith ()
fullExecutionTest file = it ("should successfully interpret " ++ takeFileName file) $ do
  code <- readFile file
  case interpreter code of
    Left err -> expectationFailure $ "Failed to interpret " ++ file ++ ": " ++ show err
    Right _ -> return ()

-- Unit test groups
parseCodeTests :: SpecWith ()
parseCodeTests = describe "parseCode" $ do
  it "parses simple integer literal" $ do
    parseCode "1" `shouldBe` Right (SLit 1)
  
  it "parses addition expression" $ do
    parseCode "1 + 2" `shouldBe` Right (SBinOp (Arith Add) (SLit 1) (SLit 2))
  
  it "fails on invalid syntax" $ do
    case parseCode "invalid code" of
      Left (InterpreterFailed _) -> return ()
      _ -> expectationFailure "Should fail on invalid code"

typeAliasExpansionTests :: SpecWith ()
typeAliasExpansionTests = describe "typeAliasExpansion" $ do
  it "leaves context unchanged" $ do
    typeAliasExpansion SCtx `shouldBe` Right SCtx
  
  it "handles alias not found error" $ do
    case typeAliasExpansion (SVar "undefinedAlias") of
      Left (InterpreterFailed msg) -> 
        msg `shouldContain` "Type Expansion Failed: some type alias not located"
      _ -> expectationFailure "Should report missing alias"

locallyNamelessTests :: SpecWith ()
locallyNamelessTests = describe "locallyNameless" $ do
  it "converts context successfully" $ do
    locallyNameless SCtx `shouldBe` Right SCtx
  
  it "detects scope errors" $ do
    case locallyNameless (SAbs "x" (SVar "y")) of
      Left (InterpreterFailed msg) -> 
        msg `shouldContain` "Locally Nameless Failed: scope errors detected"
      _ -> expectationFailure "Should detect scope error"

desugarSourceTests :: SpecWith ()
desugarSourceTests = describe "desugarSource" $ do
  it "desugars context to core" $ do
    desugarSource SCtx `shouldBe` Right TmCtx
  
  it "handles desugaring failure" $ do
    -- You'll need to create a case that fails desugaring
    pendingWith "Need example that fails desugaring"

elaborationTests :: SpecWith ()
elaborationTests = describe "elaboration" $ do
  it "elaborates context to unit type" $ do
    case elaboration TmCtx of
      Right (TySUnit, _) -> return ()
      other -> expectationFailure $ "Expected unit type, got: " ++ show other
  
  it "detects type errors during elaboration" $ do
    -- You'll need a type-incorrect example
    pendingWith "Need type-incorrect example"

evaluateTests :: SpecWith ()
evaluateTests = describe "evaluate" $ do
  it "evaluates context to unit" $ do
    evaluate Ctx `shouldBe` Right VUnit
  
  it "handles evaluation failure" $ do
    -- You'll need an example that fails evaluation
    pendingWith "Need example that fails evaluation"

errorConditionTests :: SpecWith ()
errorConditionTests = describe "Error Conditions" $ do
  it "handles IO errors in runFile" $ do
    -- Mock a file that doesn't exist
    let invalidPath = "nonexistent/file.ep"
    result <- try (runFile invalidPath) :: IO (Either IOException ())
    case result of
      Left _ -> return ()
      Right _ -> expectationFailure "Should have failed on nonexistent file"
  
  it "propagates errors through interpreter pipeline" $ do
    -- Test with code that fails at each stage
    pendingWith "Need examples that fail at each stage"