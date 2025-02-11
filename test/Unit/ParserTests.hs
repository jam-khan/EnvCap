import Test.Hspec
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
import ENVCAP.Syntax
import Text.Parsec.Expr as E (buildExpressionParser, Assoc(AssocNone), Assoc(AssocLeft), Assoc(AssocRight), Operator(Infix, Prefix) )
import Data.Functor.Identity (Identity)

mergeWrap :: Tm -> Tm
mergeWrap tm = TmMrg tm TmUnit 

test_cases :: [(String, SurfaceTm)]
test_cases = [  ("?", SCtx)
              , ("1 + 2", SBinOp (Arith Add) (SLit 1) (SLit 2))
              , ("1 - 2", SBinOp (Arith Sub) (SLit 1) (SLit 2))
              , ("3 * 4", SBinOp (Arith Mul) (SLit 3) (SLit 4))
              , ("10 / 2", SBinOp (Arith Div) (SLit 10) (SLit 2))
              , ("5 % 2", SBinOp (Arith Mod) (SLit 5) (SLit 2))
              , ("1 < 2", SBinOp (Comp Lt) (SLit 1) (SLit 2))
              , ("2 <= 3", SBinOp (Comp Le) (SLit 2) (SLit 3))
              , ("3 > 2", SBinOp (Comp Gt) (SLit 3) (SLit 2))
              , ("4 >= 1", SBinOp (Comp Ge) (SLit 4) (SLit 1))
              , ("1 == 1", SBinOp (Comp Eql) (SLit 1) (SLit 1))
              , ("2 != 3", SBinOp (Comp Neq) (SLit 2) (SLit 3))
              , ("a + b * c", SBinOp (Arith Add) (SRProj SCtx "a") (SBinOp (Arith Mul) (SRProj SCtx "b") (SRProj SCtx "c")))
              , ("1 + 2 * c", SBinOp (Arith Add) (SLit 1) (SBinOp (Arith Mul) (SLit 2) (SRProj SCtx "c")))
              , ("((3 + 4) * 2) - (5 / 2) >= (1 + x)", SBinOp   (Comp Ge)
                                                            (SBinOp (Arith Sub)
                                                                 (SBinOp (Arith Mul)
                                                                      (SBinOp (Arith Add) (SLit 3) (SLit 4))
                                                                      (SLit 2))
                                                                 (SBinOp (Arith Div) (SLit 5) (SLit 2)))
                                                            (SBinOp (Arith Add) (SLit 1) (SRProj SCtx "x")))
              , ("(x * 2) + (y / 4) >= 3", SBinOp (Comp Ge)
                                             (SBinOp (Arith Add)
                                                  (SBinOp (Arith Mul) (SRProj SCtx "x") (SLit 2))
                                                  (SBinOp (Arith Div) (SRProj SCtx "y") (SLit 4)))
                                             (SLit 3))
              , ("1 ; (x * 2) + (y / 4) >= 3", SMrg (SLit 1) (SBinOp (Comp Ge)
                                                                 (SBinOp (Arith Add)
                                                                      (SBinOp (Arith Mul) (SRProj SCtx "x") (SLit 2))
                                                                      (SBinOp (Arith Div) (SRProj SCtx "y") (SLit 4)))
                                                                 (SLit 3)))]

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