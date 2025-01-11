module ENVCAP.Parser.Lexer where

import Text.Parsec (ParseError, many1, string, try, anyChar)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit, letter)
import Text.Parsec.Combinator (eof, manyTill, anyToken, chainl1)
import Data.Char (isLetter, isDigit)
import ENVCAP.Parser.Util (lexeme)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void)

-- identifier token

identifierToken :: Parser String
identifierToken = lexeme ((:) <$> firstChar <*> many nonFirstChar)
    where
        firstChar       = letter <|> char '_'
        nonFirstChar    = digit  <|> firstChar

-- unit token

unitToken :: Parser String
unitToken = lexeme (string "()")

-- keywords token

contextToken :: Parser String
contextToken = lexeme (string "context()")

-- arithmetic operations

addToken :: Parser String
addToken = lexeme (string "+")

subToken :: Parser String
subToken = lexeme (string "-")

divToken :: Parser String
divToken = lexeme (string "/")

multToken :: Parser String
multToken = lexeme (string "*")

modToken :: Parser String
modToken = lexeme (string "%")

expToken :: Parser String
expToken = lexeme (string "^")

-- boolean values

falseToken :: Parser String
falseToken = lexeme (string "false")

trueToken :: Parser String
trueToken = lexeme (string "true")

-- comparsion operator tokens

eqlToken :: Parser String
eqlToken = lexeme (string "==")

neqToken :: Parser String
neqToken = lexeme (string "!=")

ltToken :: Parser String
ltToken = lexeme (string "<")

leToken :: Parser String
leToken = lexeme (string "<=")

gtToken :: Parser String
gtToken = lexeme (string ">")

geToken :: Parser String
geToken = lexeme (string ">=")

-- boolean operator tokens

andToken :: Parser String
andToken = lexeme (string "&&")

orToken :: Parser String
orToken  = lexeme (string "||")

notToken :: Parser String
notToken = lexeme (string "!")

-- if-conditional tokens

ifToken :: Parser String
ifToken = lexeme (string "if")

thenToken :: Parser String
thenToken = lexeme (string "then")

elseToken :: Parser String
elseToken = lexeme (string "else")

-- string token

stringToken :: Parser String
stringToken = char '\'' *> manyTill anyChar (char '\'')

