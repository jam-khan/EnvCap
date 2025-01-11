module ENVCAP.Parser.Parser where

import ENVCAP.Source.Syntax (Tm(..), Typ(..), TmBinaryOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))
import Text.Parsec (ParseError, many1, string, try, between, anyChar, notFollowedBy, lookAhead, Parsec)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit, letter, noneOf)
import Text.Parsec.Combinator (eof, manyTill, option, anyToken, chainl1)
import Data.Char (isLetter, isDigit)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void, guard)
import ENVCAP.Parser.Util
    ( falseToken,
      identifierToken,
      keyword,
      lexeme,
      parseWithWhitespace,
      stringToken,
      trueToken,
      unitToken ) 
import ENVCAP.Core.Syntax (Exp(BinOp))
import Text.Parsec.Expr as E (buildExpressionParser, Assoc(AssocNone), Assoc(AssocLeft), Assoc(AssocRight), Operator(Infix, Prefix) )
import Data.Functor.Identity (Identity)


-- Parser for context

parseCtx        :: Parser Tm
parseCtx        = lexeme $ keyword "context()" >> return TmQuery

parseTrue       :: Parser Tm
parseTrue       = lexeme $ trueToken    >> return (TmBool True)

parseFalse      :: Parser Tm
parseFalse      = lexeme $ falseToken   >> return (TmBool False)

parseBoolean    :: Parser Tm
parseBoolean    = try parseTrue <|> parseFalse

parseString :: Parser Tm
parseString = TmString <$> lexeme stringToken

-- Parser for integer literals

parseInteger :: Parser Tm
parseInteger = lexeme $ do
                        sign    <-  option "" (string "-")  -- Handling Prefix -
                        void    $   option "" (string "+")   -- Handling Prefix +
                        num     <-  read <$> many1 digit
                        return $ TmInt (if null sign then num else -num)
-- Parser for unit

parseUnit   :: Parser Tm
parseUnit   = lexeme    $ void unitToken >> return TmUnit

-- Parser for variable

parseVar    :: Parser Tm
parseVar    = TmVar     <$> identifierToken


-- Parser for logical operations
operationParser :: Parsec String () Tm
operationParser = lexeme $ buildExpressionParser operators parseTerm

operators :: [[Operator String () Identity Tm]]
operators =
        [
                [E.Prefix (TmUnary TmNot            <$ char '!')],
                [E.Infix (TmBinary (TmArith TmExp)  <$ symbol "^") E.AssocLeft],
                [E.Infix (TmBinary (TmArith TmMod)  <$ symbol "%") E.AssocLeft,
                 E.Infix (TmBinary (TmArith TmMul)  <$ symbol "*") E.AssocLeft,
                 E.Infix (TmBinary (TmArith TmDiv)  <$ symbol "/") E.AssocLeft],

                [E.Infix (TmBinary (TmArith TmAdd)  <$ symbol "+") E.AssocLeft,
                 E.Infix (TmBinary (TmArith TmSub)  <$ symbol "-") E.AssocLeft],

                [E.Infix (TmBinary (TmComp TmLt)    <$ symbol "<")  E.AssocNone,
                 E.Infix (TmBinary (TmComp TmGt)    <$ symbol ">")  E.AssocNone],

                [E.Infix (TmBinary (TmComp TmGe)    <$ symbol ">=") E.AssocRight,
                 E.Infix (TmBinary (TmComp TmLe)    <$ symbol "<=") E.AssocRight,
                 E.Infix (TmBinary (TmComp TmEql)   <$ symbol "==") E.AssocRight,
                 E.Infix (TmBinary (TmComp TmNeq)   <$ symbol "!=") E.AssocRight],

                [E.Infix (TmBinary (TmLogic TmAnd)  <$ symbol "&&") E.AssocRight],

                [E.Infix (TmBinary (TmLogic TmOr)   <$ symbol "||")   E.AssocRight]
        ]


parseTerm :: Parser Tm
parseTerm = try         parseCtx
                <|>     parseInteger
                <|>     parseString
                <|>     parseBoolean
                <|>     parseVar
                <|>     parens operationParser
                <|>     parseUnit

parseExp :: Parser Tm
parseExp =      parseConditional
        <|>     operationParser
        <|>     parseCtx
        <|>     parseUnit
        <|>     parseVar
        <|>     parseBoolean
        <|>     parseInteger
        <|>     parseString

symbol :: String -> Parser String
symbol s = try $ lexeme $ do
                u <- many1 (oneOf "<>=+-^%/*!&|")
                guard (s == u)
                return s

parens :: Parser Tm -> Parser Tm
parens p = lexeme $ between (char '(') (char ')') p

parseConditional :: Parser Tm
parseConditional = TmIf2    <$> (void (keyword "if")       *> parseExp)
                            <*> (void (keyword "then")     *> parseExp)
                            <*> (void (keyword "else")     *> parseExp)

-- main parser
parseMain :: String -> Either ParseError Tm
parseMain = parseWithWhitespace parseExp
