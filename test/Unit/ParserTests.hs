
import Test.Hspec
import ENVCAP.Source.Syntax (Tm(..), Typ(..), TmBinaryOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))
import ENVCAP.Parser.Parser (parseMain)
import Text.Parsec (ParseError, many1, string, try, between, anyChar, notFollowedBy, lookAhead, Parsec)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit, letter, noneOf)
import Text.Parsec.Combinator (eof, manyTill, option, anyToken, chainl1)
import Data.Char (isLetter, isDigit)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad ( void, guard, forM_, forM_ )
import ENVCAP.Parser.Util (lexeme, parseWithWhitespace)
import ENVCAP.Core.Syntax (Exp(BinOp))
import Text.Parsec.Expr as E (buildExpressionParser, Assoc(AssocNone), Assoc(AssocLeft), Assoc(AssocRight), Operator(Infix, Prefix) )
import Data.Functor.Identity (Identity)

test_cases :: [(String, Tm)]
test_cases =
    [ ("context()", TmQuery)
    , ("(1)", TmInt 1)
    , ("(false)", TmBool False)
    , ("true", TmBool True)
    , ("'hello'", TmString "hello")
    , ("1 + 2", TmBinary (TmArith TmAdd) (TmInt 1) (TmInt 2))
    , ("1 - 2", TmBinary (TmArith TmSub) (TmInt 1) (TmInt 2))
    , ("3 * 4", TmBinary (TmArith TmMul) (TmInt 3) (TmInt 4))
    , ("10 / 2", TmBinary (TmArith TmDiv) (TmInt 10) (TmInt 2))
    , ("5 % 2", TmBinary (TmArith TmMod) (TmInt 5) (TmInt 2))
    , ("1 < 2", TmBinary (TmComp TmLt) (TmInt 1) (TmInt 2))
    , ("2 <= 3", TmBinary (TmComp TmLe) (TmInt 2) (TmInt 3))
    , ("3 > 2", TmBinary (TmComp TmGt) (TmInt 3) (TmInt 2))
    , ("4 >= 1", TmBinary (TmComp TmGe) (TmInt 4) (TmInt 1))
    , ("1 == 1", TmBinary (TmComp TmEql) (TmInt 1) (TmInt 1))
    , ("2 != 3", TmBinary (TmComp TmNeq) (TmInt 2) (TmInt 3))
    , ("if true then 1 else 0", TmIf2 (TmBool True) (TmInt 1) (TmInt 0))
    , ("if false then 1 + 2 else 2 - 1", TmIf2
        (TmBool False)
        (TmBinary (TmArith TmAdd) (TmInt 1) (TmInt 2))
        (TmBinary (TmArith TmSub) (TmInt 2) (TmInt 1)))
    , ("a + b * c", TmBinary (TmArith TmAdd) (TmVar "a") (TmBinary (TmArith TmMul) (TmVar "b") (TmVar "c")))
    , ("if x < y then z else w", TmIf2
        (TmBinary (TmComp TmLt) (TmVar "x") (TmVar "y"))
        (TmVar "z")
        (TmVar "w"))
    , ("if (1 + 2) > 2 then context() else false",
        TmIf2
            (TmBinary (TmComp TmGt)
                (TmBinary (TmArith TmAdd) (TmInt 1) (TmInt 2))
                (TmInt 2))
            TmQuery
            (TmBool False))
    , ("(x * 2) + (y / 4) >= 3",
        TmBinary (TmComp TmGe)
            (TmBinary (TmArith TmAdd)
                (TmBinary (TmArith TmMul) (TmVar "x") (TmInt 2))
                (TmBinary (TmArith TmDiv) (TmVar "y") (TmInt 4)))
            (TmInt 3))
    ,  ("if false then 1 else (2 * 3) + 1",
        TmIf2
            (TmBool False)
            (TmInt 1)
            (TmBinary (TmArith TmAdd)
                (TmBinary (TmArith TmMul) (TmInt 2) (TmInt 3))
                (TmInt 1)))
     , ("((3 + 4) * 2) - (5 / 2) >= (1 + x)",
        TmBinary (TmComp TmGe)
            (TmBinary (TmArith TmSub)
                (TmBinary (TmArith TmMul)
                    (TmBinary (TmArith TmAdd) (TmInt 3) (TmInt 4))
                    (TmInt 2))
                (TmBinary (TmArith TmDiv) (TmInt 5) (TmInt 2)))
            (TmBinary (TmArith TmAdd) (TmInt 1) (TmVar "x")))
      , ( "if (a * 2 + (3 < 4) * 5) >= (b / 2) then context() else (1 + (2 * 3) - (4 / x))",
        TmIf2
                (TmBinary (TmComp TmGe)
                (TmBinary (TmArith TmAdd)
                        (TmBinary (TmArith TmMul) (TmVar "a") (TmInt 2))
                        (TmBinary (TmArith TmMul)
                        (TmBinary (TmComp TmLt) (TmInt 3) (TmInt 4))
                        (TmInt 5)))
                (TmBinary (TmArith TmDiv) (TmVar "b") (TmInt 2)))
                TmQuery
                (TmBinary (TmArith TmSub)
                (TmBinary (TmArith TmAdd) (TmInt 1)
                        (TmBinary (TmArith TmMul) (TmInt 2) (TmInt 3)))
                (TmBinary (TmArith TmDiv) (TmInt 4) (TmVar "x")))
    )]

tester :: [(String, Tm)] -> Bool
tester [] = True
tester (x:xs) = case parseMain (fst x) of
                    Right res -> res == snd x && tester xs
                    _         -> False

test :: IO ()
test = do
        if tester test_cases
                then putStrLn $ show (length test_cases) ++ " test cases passed!!"
                else putStrLn "Tests Failed"


-- Define the test function
parserTests :: Spec
parserTests = describe "Parser Tests" $ do
    forM_ test_cases $ \(input, expectedOutput) -> do
        it ("parses: " ++ input) $ do
            parseMain input `shouldBe` Right expectedOutput

-- Main function to run tests
main :: IO ()
main = hspec parserTests