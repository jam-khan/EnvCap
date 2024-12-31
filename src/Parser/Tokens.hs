module Parser.Tokens where

import Text.Parsec (ParseError, many1, string, try, anyChar)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit, letter)
import Text.Parsec.Combinator (eof, manyTill, anyToken, chainl1)
import Data.Char (isLetter, isDigit)
import Parser.Util (lexeme)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void)

-- identifier token

identifierToken :: Parser String
identifierToken = lexeme ((:) <$> firstChar <*> many nonFirstChar)
    where
        firstChar       = letter <|> char '_'
        nonFirstChar    = digit  <|> firstChar

-- unit token

unitToken :: Parser ()
unitToken = void $ string "()"

-- keywords token

contextToken :: Parser ()
contextToken = void $ string "context()"

-- arithmetic operations

addToken :: Parser ()
addToken = void $ string "+"

subToken :: Parser ()
subToken = void $ string "-"

divToken :: Parser ()
divToken = void $ string "/"

multToken :: Parser ()
multToken = void $ string "*"

modToken :: Parser ()
modToken = void $ string "%"

-- boolean values

falseToken :: Parser ()
falseToken = void $ string "false"

trueToken :: Parser ()
trueToken = void $ string "true"

-- logical operator tokens

eqlToken :: Parser ()
eqlToken = void $ string "=="

neqToken :: Parser ()
neqToken = void $ string "!="

ltToken :: Parser ()
ltToken = void $ string "<"

leToken :: Parser ()
leToken = void $ string "<="

gtToken :: Parser ()
gtToken = void $ string ">"


geToken :: Parser ()
geToken = void $ string ">="

-- boolean operator tokens

andToken :: Parser ()
andToken = void $ string "&&"

orToken :: Parser ()
orToken  = void $ string "||"

notToken :: Parser ()
notToken = void $ string "!"

-- if-conditional tokens

ifToken :: Parser ()
ifToken = void $ string "if"

thenToken :: Parser ()
thenToken = void $ string "then"

elseToken :: Parser ()
elseToken = void $ string "then"

-- string token

stringToken :: Parser String
stringToken = char '\'' *> manyTill anyChar (char '\'')

