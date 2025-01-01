module Parser.Parser where

import Surface.Syntax (Tm(..), Typ(..), TmBinaryOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))
import Parser.Lexer (identifierToken, trueToken, falseToken, contextToken, unitToken, addToken, subToken, multToken, divToken, modToken, andToken, orToken, ifToken, thenToken, elseToken, stringToken, notToken)
import Text.Parsec (ParseError, many1, string, try, between, anyChar, notFollowedBy, lookAhead, Parsec)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit, letter, noneOf)
import Text.Parsec.Combinator (eof, manyTill, option, anyToken, chainl1)
import Data.Char (isLetter, isDigit)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void, guard)
import Parser.Util (lexeme, parseWithWhitespace)
import Core.Syntax (Exp(BinOp))
import Text.Parsec.Expr as E (buildExpressionParser, Assoc(AssocNone), Assoc(AssocLeft), Assoc(AssocRight), Operator(Infix, Prefix) )
import Data.Functor.Identity (Identity)

-- Parse 

examples :: [(String, Tm)]
examples = 
    [ ("context()", TmCtx)
    , ("1", TmInt 1)
    , ("false", TmBool False)
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
    , ("if true then 1 else 0", TmIf (TmBool True) (TmInt 1) (TmInt 0))
    , ("if false then 1 + 2 else 2 - 1", TmIf 
        (TmBool False) 
        (TmBinary (TmArith TmAdd) (TmInt 1) (TmInt 2)) 
        (TmBinary (TmArith TmSub) (TmInt 2) (TmInt 1)))
    , ("a + b * c", TmBinary (TmArith TmAdd) (TmVar "a") (TmBinary (TmArith TmMul) (TmVar "b") (TmVar "c")))
    , ("if x < y then z else w", TmIf 
        (TmBinary (TmComp TmLt) (TmVar "x") (TmVar "y")) 
        (TmVar "z") 
        (TmVar "w"))]

tester :: [(String, Tm)] -> Bool
tester [] = True
tester (x:xs) = case parseMain (fst x) of
                Right res -> res == (snd x) && tester xs
                _         -> False

-- Parser for context

parseCtx :: Parser Tm
parseCtx = lexeme $ (keyword "context()") >> return TmCtx

-- Parser for boolean literals

parseTrue :: Parser Tm
parseTrue = lexeme $ trueToken >> return (TmBool True)

parseFalse :: Parser Tm
parseFalse = lexeme $ falseToken >> return (TmBool False)

parseBoolean :: Parser Tm
parseBoolean  = try parseTrue <|> parseFalse


-- Parser for integer literals

parseInteger :: Parser Tm
parseInteger = lexeme $ do
                        sign <- option "" (string "-")  -- Handling Prefix -
                        void $ option "" (string "+")   -- Handling Prefix +
                        num  <- read <$> (many1 digit)
                        return $ TmInt (if null sign then num else -num)
-- Parser for unit

parseUnit :: Parser Tm
parseUnit = lexeme $ void unitToken >> return TmUnit

-- Parser for variable

parseVar :: Parser Tm
parseVar = TmVar <$> identifierToken


-- Parser for logical operations
operationParser :: Parsec String () Tm
operationParser = lexeme $ buildExpressionParser operators parseTerm

operators :: [[Operator String () Identity Tm]]
operators = 
        [
                [E.Prefix ((TmUnary TmNot) <$ char '!')],

                [E.Infix (TmBinary (TmArith TmExp) <$ symbol "^") E.AssocLeft],

                [E.Infix (TmBinary (TmArith TmMod) <$ symbol "%") E.AssocLeft,
                 E.Infix (TmBinary (TmArith TmMul) <$ symbol "*") E.AssocLeft,
                 E.Infix (TmBinary (TmArith TmDiv) <$ symbol "/") E.AssocLeft],

                [E.Infix (TmBinary (TmArith TmAdd) <$ symbol "+") E.AssocLeft,
                 E.Infix (TmBinary (TmArith TmSub) <$ symbol "-") E.AssocLeft],
                
                [E.Infix (TmBinary (TmComp TmLt)  <$ symbol "<")  E.AssocNone,
                 E.Infix (TmBinary (TmComp TmGt)  <$ symbol ">")  E.AssocNone],

                [E.Infix (TmBinary (TmComp TmGe)  <$ symbol ">=") E.AssocRight,
                 E.Infix (TmBinary (TmComp TmLe)  <$ symbol "<=") E.AssocRight,
                 E.Infix (TmBinary (TmComp TmEql) <$ symbol "==") E.AssocRight,
                 E.Infix (TmBinary (TmComp TmNeq) <$ symbol "!=") E.AssocRight],

                [E.Infix (TmBinary (TmLogic TmAnd) <$ symbol "&&") E.AssocRight],

                [E.Infix (TmBinary (TmLogic TmOr) <$ symbol "||")   E.AssocRight]
        ]

nonWhitespace :: Parser String
nonWhitespace = many (noneOf " \t\n\r")

keyword :: String -> Parser String
keyword k = try $ lexeme (string k <* notFollowedBy nonWhitespace)

parseTerm :: Parser Tm
parseTerm = try parseCtx <|> parseInteger <|> parseString <|> parseUnit <|> parseBoolean <|> parseVar

parseExp :: Parser Tm
parseExp =          parseConditional    <|> operationParser     
                <|> parseCtx            <|> parseUnit           <|> parseVar 
                <|> parseBoolean        <|> parseInteger        <|> parseString 

symbol :: String -> Parser String
symbol s = try $ lexeme $ do
                u <- many1 (oneOf "<>=+-^%/*!|")
                guard (s == u)
                return s

parseConditional :: Parser Tm
parseConditional = TmIf <$> (void (keyword "if")       *> parseExp)
                        <*> (void (keyword "then")     *> parseExp)
                        <*> (void (keyword "else")     *> parseExp)

-- parser for string
parseString :: Parser Tm
parseString = TmString <$> lexeme stringToken

-- main parser
parseMain :: String -> Either ParseError Tm
parseMain s = parseWithWhitespace parseExp s
