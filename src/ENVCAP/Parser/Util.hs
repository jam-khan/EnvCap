module ENVCAP.Parser.Util where

import Text.Parsec (ParseError, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import ENVCAP.Syntax 

-- We want to comments to be considered as whitespace.
whitespace :: Parser ()
whitespace =
    choice [simpleWhitespace *> whitespace
           ,lineComment *> whitespace
           ,blockComment *> whitespace
           ,return ()]
  where
    lineComment = try (string "--")
                  *> manyTill anyChar (void (char '\n') <|> eof)
    blockComment = try (string "/*")
                   *> manyTill anyChar (try $ string "*/")
    simpleWhitespace = void $ many1 (oneOf " \t\n\r")

regularParse :: Parser a -> String -> Either ParseError a
regularParse p  = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p  = parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
    where leftOver = manyTill anyToken eof

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

lexeme          :: Parser a -> Parser a
lexeme p        = p <* whitespace

nonWhitespace   :: Parser String
nonWhitespace   = many (noneOf " \t\n\r")

keyword         :: String -> Parser String
keyword k       = try $ lexeme (string k <* notFollowedBy nonWhitespace)

identifierToken     :: Parser String
identifierToken     = lexeme ((:) <$> firstChar <*> many nonFirstChar)
                        where
                            firstChar       = letter <|> char '_'
                            nonFirstChar    = digit  <|> firstChar

intersections :: [TypS] -> TypS
intersections []  = TySUnit
intersections [x] = x
intersections (x:xs) = TySAnd x $ intersections xs

merges :: [Tm] -> Tm
merges [] = TmUnit
merges [x] = x
merges (x:xs) = TmMrg x $ merges xs

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` lexeme (char ','))

contextToken    :: Parser String
contextToken    = lexeme (string "?")

unitToken   :: Parser String
unitToken   = lexeme (string "()")

addToken    :: Parser String
addToken    = lexeme (string "+")

subToken    :: Parser String
subToken    = lexeme (string "-")

divToken    :: Parser String
divToken    = lexeme (string "/")

multToken   :: Parser String
multToken   = lexeme (string "*")

modToken    :: Parser String
modToken    = lexeme (string "%")

expToken    :: Parser String
expToken    = lexeme (string "^")

falseToken  :: Parser String
falseToken  = lexeme (string "false")

trueToken   :: Parser String
trueToken   = lexeme (string "true")

eqlToken    :: Parser String
eqlToken    = lexeme (string "==")

neqToken    :: Parser String
neqToken    = lexeme (string "!=")

ltToken     :: Parser String
ltToken     = lexeme (string "<")

leToken     :: Parser String
leToken     = lexeme (string "<=")

gtToken     :: Parser String
gtToken     = lexeme (string ">")

geToken     :: Parser String
geToken     = lexeme (string ">=")

andToken    :: Parser String
andToken    = lexeme (string "&&")

orToken     :: Parser String
orToken     = lexeme (string "||")

notToken    :: Parser String
notToken    = lexeme (string "!")

ifToken     :: Parser String
ifToken     = lexeme (string "if")

thenToken   :: Parser String
thenToken   = lexeme (string "then")

elseToken   :: Parser String
elseToken   = lexeme (string "else")

stringToken :: Parser String
stringToken = char '\'' *> manyTill anyChar (char '\'')

