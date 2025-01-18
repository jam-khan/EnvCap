
import Test.Hspec
import ENVCAP.Source.Syntax (Tm(..), Typ(..), TmBinOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))
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

mergeWrap :: Tm -> Tm
mergeWrap tm = TmMrg tm TmUnit 

test_cases :: [(String, Tm)]
test_cases =
    [ ("?", mergeWrap TmCtx)
    , ("(1)", mergeWrap $ TmLit 1)
    , ("(false)", mergeWrap $ TmBool False)
    , ("true", mergeWrap $ TmBool True)
    , ("'hello'", mergeWrap $ TmString "hello")
    , ("1 + 2", mergeWrap $ TmBinOp (TmArith TmAdd) (TmLit 1) (TmLit 2))
    , ("1 - 2", mergeWrap $ TmBinOp (TmArith TmSub) (TmLit 1) (TmLit 2))
    , ("3 * 4", mergeWrap $ TmBinOp (TmArith TmMul) (TmLit 3) (TmLit 4))
    , ("10 / 2", mergeWrap $ TmBinOp (TmArith TmDiv) (TmLit 10) (TmLit 2))
    , ("5 % 2", mergeWrap $ TmBinOp (TmArith TmMod) (TmLit 5) (TmLit 2))
    , ("1 < 2", mergeWrap $ TmBinOp (TmComp TmLt) (TmLit 1) (TmLit 2))
    , ("2 <= 3", mergeWrap $ TmBinOp (TmComp TmLe) (TmLit 2) (TmLit 3))
    , ("3 > 2", mergeWrap $ TmBinOp (TmComp TmGt) (TmLit 3) (TmLit 2))
    , ("4 >= 1", mergeWrap $ TmBinOp (TmComp TmGe) (TmLit 4) (TmLit 1))
    , ("1 == 1", mergeWrap $ TmBinOp (TmComp TmEql) (TmLit 1) (TmLit 1))
    , ("2 != 3", mergeWrap $ TmBinOp (TmComp TmNeq) (TmLit 2) (TmLit 3))
    , ("if true then 1 else 0", mergeWrap $ TmIf (TmBool True) (TmLit 1) (TmLit 0))
    , ("if false then 1 + 2 else 2 - 1", mergeWrap $ TmIf
        (TmBool False)
        (TmBinOp (TmArith TmAdd) (TmLit 1) (TmLit 2))
        (TmBinOp (TmArith TmSub) (TmLit 2) (TmLit 1)))
    , ("a + b * c", mergeWrap $ TmBinOp (TmArith TmAdd) (TmVar "a") (TmBinOp (TmArith TmMul) (TmVar "b") (TmVar "c")))
    , ("if x < y then z else w", mergeWrap $ TmIf
        (TmBinOp (TmComp TmLt) (TmVar "x") (TmVar "y"))
        (TmVar "z")
        (TmVar "w"))
    , ("if (1 + 2) > 2 then ? else false",
        mergeWrap $ TmIf
            (TmBinOp (TmComp TmGt)
                (TmBinOp (TmArith TmAdd) (TmLit 1) (TmLit 2))
                (TmLit 2))
            TmCtx
            (TmBool False))
    , ("(x * 2) + (y / 4) >= 3",
        mergeWrap $ TmBinOp (TmComp TmGe)
            (TmBinOp (TmArith TmAdd)
                (TmBinOp (TmArith TmMul) (TmVar "x") (TmLit 2))
                (TmBinOp (TmArith TmDiv) (TmVar "y") (TmLit 4)))
            (TmLit 3))
    ,  ("if false then 1 else (2 * 3) + 1",
        mergeWrap $ TmIf
            (TmBool False)
            (TmLit 1)
            (TmBinOp (TmArith TmAdd)
                (TmBinOp (TmArith TmMul) (TmLit 2) (TmLit 3))
                (TmLit 1)))
     , ("((3 + 4) * 2) - (5 / 2) >= (1 + x)",
        mergeWrap $ TmBinOp (TmComp TmGe)
            (TmBinOp (TmArith TmSub)
                (TmBinOp (TmArith TmMul)
                    (TmBinOp (TmArith TmAdd) (TmLit 3) (TmLit 4))
                    (TmLit 2))
                (TmBinOp (TmArith TmDiv) (TmLit 5) (TmLit 2)))
            (TmBinOp (TmArith TmAdd) (TmLit 1) (TmVar "x")))
      , ( "if (a * 2 + (3 < 4) * 5) >= (b / 2) then ? else (1 + (2 * 3) - (4 / x))",
        mergeWrap $ TmIf
                (TmBinOp (TmComp TmGe)
                (TmBinOp (TmArith TmAdd)
                        (TmBinOp (TmArith TmMul) (TmVar "a") (TmLit 2))
                        (TmBinOp (TmArith TmMul)
                        (TmBinOp (TmComp TmLt) (TmLit 3) (TmLit 4))
                        (TmLit 5)))
                (TmBinOp (TmArith TmDiv) (TmVar "b") (TmLit 2)))
                TmCtx
                (TmBinOp (TmArith TmSub)
                (TmBinOp (TmArith TmAdd) (TmLit 1)
                        (TmBinOp (TmArith TmMul) (TmLit 2) (TmLit 3)))
                (TmBinOp (TmArith TmDiv) (TmLit 4) (TmVar "x")))
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