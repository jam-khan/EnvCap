module Parser.Util where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, space, newline, tab, oneOf, anyChar)
import Text.Parsec.Combinator (eof, manyTill, anyToken)
import Control.Applicative ((<$>), (<*>), (<*), (*>), many)
import Data.Char (isLower)
import Control.Monad (void)
import qualified Data.Bifunctor


-- Basic wrapper
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

-- Wrapper with fail if all input is not consumed
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

-- Wrapper to apply parser and then, return remaining string
parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
        where leftOver = manyTill anyToken eof
