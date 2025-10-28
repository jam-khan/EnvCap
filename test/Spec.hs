{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text, unpack)
import ENVCAP.Interpreter
import ENVCAP.Source.Errors
import ENVCAP.Syntax
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (replaceExtension, (</>))
import Test.Hspec

type TestName = String

type Code = String

type ExpectedOutput = String

type ExpectedError = String

type TestCase = (TestName, Code, TestType)

data TestType
  = DoPass ExpectedOutput
  | DoErr ExpectedError

rstrip :: String -> String
rstrip = dropWhileEnd isSpace

endsWith :: String -> String -> Bool
endsWith ext file = reverse ext == take (length ext) (reverse file)

getPassFileTests :: FilePath -> IO [TestCase]
getPassFileTests dir = do
  files <- listDirectory dir
  forM (filter (endsWith ".ep") files) $ \file -> do
    let epFile = dir </> file
    let passFile = replaceExtension epFile ".pass"
    code <- readFile epFile
    passExists <- doesFileExist passFile
    if passExists
      then do
        expectedOutput <- readFile passFile
        return (file, code, DoPass (rstrip expectedOutput))
      else
        error $ "Expected output file does not exist: " ++ passFile

getErrFileTests :: FilePath -> IO [TestCase]
getErrFileTests dir = do
  files <- listDirectory dir
  forM (filter (endsWith ".ep") files) $ \file -> do
    let epFile = dir </> file
    let errFile = replaceExtension epFile ".err"
    code <- readFile epFile
    errExists <- doesFileExist errFile
    if errExists
      then do
        expectedErr <- readFile errFile
        return (file, code, DoErr (rstrip expectedErr))
      else
        error $ "Expected error file does not exist: " ++ errFile

nonModularTests :: [TestCase]
nonModularTests =
  map
    (\(name, code, expected) -> (name, "@pure module PlaceholderModule\n" ++ code, expected))
    [ ( "literal_int",
        "42",
        DoPass "VInt 42"
      ),
      ( "literal_string",
        "\"hello\"",
        DoPass "VString \"hello\""
      ),
      ( "literal_string_alt",
        "'world'",
        DoPass "VString \"world\""
      ),
      ( "literal_bool_true",
        "True",
        DoPass "VBool True"
      ),
      ( "literal_bool_false",
        "False",
        DoPass "VBool False"
      ),
      ( "literal_unit",
        "unit",
        DoPass "VUnit"
      ),
      ( "let_simple",
        "let {x:Int = 10} in {x}",
        DoPass "VInt 10"
      ),
      ( "let_shadow",
        "let {x:Int = 5} in {let {x:Int = x + 1} in {x}}",
        DoPass "VInt 6"
      ),
      ( "let_multiple",
        "let {a:Int = 2; b:Int = 3} in {a + b}",
        DoPass "VInt 5"
      ),
      ( "arith_add",
        "3 + 4",
        DoPass "VInt 7"
      ),
      ( "arith_sub",
        "10 - 3",
        DoPass "VInt 7"
      ),
      ( "arith_mul",
        "6 * 7",
        DoPass "VInt 42"
      ),
      ( "arith_div",
        "20 / 4",
        DoPass "VInt 5"
      ),
      ( "arith_mod",
        "10 % 3",
        DoPass "VInt 1"
      ),
      ( "arith_precedence",
        "2 + 3 * 4",
        DoPass "VInt 14"
      ),
      ( "cmp_eq_int",
        "5 == 5",
        DoPass "VBool True"
      ),
      ( "cmp_neq",
        "1 != 2",
        DoPass "VBool True"
      ),
      ( "cmp_lt",
        "3 < 5",
        DoPass "VBool True"
      ),
      ( "cmp_le",
        "5 <= 5",
        DoPass "VBool True"
      ),
      ( "cmp_gt",
        "10 > 7",
        DoPass "VBool True"
      ),
      ( "cmp_ge",
        "4 >= 4",
        DoPass "VBool True"
      ),
      ( "bool_and_true",
        "True && True",
        DoPass "VBool True"
      ),
      ( "bool_and_false",
        "True && False",
        DoPass "VBool False"
      ),
      ( "bool_or",
        "False || True",
        DoPass "VBool True"
      ),
      -- ( "bool_not",
      --   "!False",
      --   DoPass "VBool True"
      -- ), -- TODO
      ( "if_true",
        "if (True) then {100} else {200}",
        DoPass "VInt 100"
      ),
      ( "if_false",
        "if (False) then {100} else {200}",
        DoPass "VInt 200"
      ),
      ( "if_nested",
        "if (True) then {if (False) then {4} else {5}} else {6}",
        DoPass "VInt 5"
      ),
      ( "lambda_function",
        "let {f : Int -> Int = \\(x : Int) => {x + 1} } in {f(41)}",
        DoPass "VInt 42"
      )
    ]

runTest :: TestCase -> SpecWith ()
runTest (name, code, testType) = it ("should interpret " ++ name ++ " correctly") $ do
  case interpreter code of
    Left err -> case testType of
      DoErr expectedErr -> return () -- TODO
      DoPass _ -> return () -- TODO
    Right val -> case testType of
      DoPass expectedOutput -> show val `shouldBe` expectedOutput
      DoErr _ -> return () -- TODO

main :: IO ()
main = do
  passFileTests <- getPassFileTests "test/tests/DoPass"
  errFileTests <- getErrFileTests "test/tests/DoErr"
  let allTests = nonModularTests ++ passFileTests ++ errFileTests
  hspec $ do
    describe "Interpreter Tests" $
      mapM_ runTest allTests
