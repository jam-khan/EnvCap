module ENVCAP.Parser.Util where

import Text.Parsec (ParseError, many1)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit)
import Text.Parsec.Combinator (eof, manyTill, anyToken)
import Data.Char (isLetter, isDigit)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void)

-- | Consumes whitespace characters.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- | Runs a parser on an input string.
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

-- | Fails if the entire input is not consumed.
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

-- | Returns parsed result and any remaining input as a tuple.
parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
    where leftOver = manyTill anyToken eof

-- | Ignores leading whitespace before applying the parser.
parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

-- | Applies a parser and consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
