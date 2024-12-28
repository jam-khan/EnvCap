{- |
Module      : Parser.Util
Description : Utility functions for parsing with Parsec.
Copyright   : (c) 2024, Jam Kabeer Ali Khan
License     : MIT

Maintainer  : jamkhan@connect.hku.hk
Stability   : Development

This module provides utility functions for the Parsec library, simplifying common parsing tasks.

Key Functions:
- 'whitespace': Consumes whitespace characters.
- 'regularParse': Runs a parser on an input string.
- 'parseWithEof': Fails if the entire input is not consumed.
- 'parseWithLeftOver': Returns parsed result and any remaining input.
- 'parseWithWhitespace': Ignores leading whitespace before applying the parser.
- 'lexeme': Applies a parser and consumes trailing whitespace.

Example Usage:
>>> regularParse (char 'a') "abc"
Right 'a'

>>> parseWithEof (many (satisfy isLower)) "abc"
Right "abc"

>>> parseWithLeftOver (many digit) "123abc"
Right ("123", "abc")

>>> parseWithWhitespace (char 'a') "   a"
Right 'a'
-}

module Parser.Util where

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
