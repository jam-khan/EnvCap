module ENVCAP.Parser.Parser where
import System.IO.Error (catchIOError)
import ENVCAP.Source.Syntax (Tm(..), Typ(..), TmBinOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))
import Text.Parsec (ParseError, many1, string, try, between, anyChar, notFollowedBy, lookAhead, Parsec, sepEndBy1)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit, letter, noneOf)
import Text.Parsec.Combinator (eof, manyTill, option, anyToken, chainl1, choice, sepBy, sepEndBy1)
import Data.Char (isLetter, isDigit)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void, guard)
import ENVCAP.Parser.Util
import Text.Parsec.Expr as E (buildExpressionParser, Assoc(AssocNone), Assoc(AssocLeft), Assoc(AssocRight), Operator(Infix, Prefix) )
import Data.Functor.Identity (Identity)
import ENVCAP.Core.Util (merge)


parseCtx        :: Parser Tm
parseCtx        = lexeme $ keyword "?" >> return TmCtx

parseTrue       :: Parser Tm
parseTrue       = lexeme $ trueToken    >> return (TmBool True)

parseFalse      :: Parser Tm
parseFalse      = lexeme $ falseToken   >> return (TmBool False)

parseBoolean    :: Parser Tm
parseBoolean    = try parseTrue <|> parseFalse

parseString     :: Parser Tm
parseString     = TmString <$> lexeme stringToken

parseInteger :: Parser Tm
parseInteger = lexeme $ do
                        sign    <-  option "" (string "-")      -- Handling Prefix -
                        void    $   option "" (string "+")      -- Handling Prefix +
                        num     <-  read <$> many1 digit
                        return $ TmLit (if null sign then num else -num)

parseUnit   :: Parser Tm
parseUnit   = lexeme    $ void unitToken >> return TmUnit

parseDef    :: Parser Tm
parseDef    = TmRProj TmCtx <$> identifierToken

parseAssign :: Parser Tm
parseAssign = do
                void (lexeme $ keyword "define")
                name <- identifierToken
                void (lexeme $ char '=')
                TmRec name <$> parseExp

parseType :: Parser Typ
parseType = do
                void (lexeme $ string "Int")
                return TInt

parseLambdaParams :: Parser [Typ]
parseLambdaParams = parseParam `sepEndBy1` lexeme (char ',')

parseParam :: Parser Typ
parseParam = do
    name <- identifierToken
    void (lexeme $ char ':')
    TRecord name <$> parseType


parseLambda :: Parser Tm
parseLambda = do
                void (lexeme $ string "\\(")
                ty <- lexeme parseLambdaParams
                void (lexeme $ char ')')
                void (lexeme $ symbol "=>")
                tm <- lexeme $ between (lexeme $ char '{') (lexeme $ char '}') parseMultExpr
                return $ TmLam (intersections ty) (merges tm)

parseApplication :: Parser Tm
parseApplication = do
                        funcName <- identifierToken
                        void $ lexeme $ char '('
                        exp <- lexeme parseExp
                        void $ lexeme $ char ')'
                        return $ TmApp (TmRProj TmCtx funcName) exp


operationParser :: Parsec String () Tm
operationParser = lexeme $ buildExpressionParser operators parseTerm

operators :: [[Operator String () Identity Tm]]
operators =    [[E.Prefix (TmUnOp TmNot            <$ char '!')],
                [E.Infix (TmBinOp (TmArith TmExp)  <$ symbol "^") E.AssocLeft],
                [E.Infix (TmBinOp (TmArith TmMod)  <$ symbol "%") E.AssocLeft,
                 E.Infix (TmBinOp (TmArith TmMul)  <$ symbol "*") E.AssocLeft,
                 E.Infix (TmBinOp (TmArith TmDiv)  <$ symbol "/") E.AssocLeft],

                [E.Infix (TmBinOp (TmArith TmAdd)  <$ symbol "+") E.AssocLeft,
                 E.Infix (TmBinOp (TmArith TmSub)  <$ symbol "-") E.AssocLeft],

                [E.Infix (TmBinOp (TmComp TmLt)    <$ symbol "<")  E.AssocNone,
                 E.Infix (TmBinOp (TmComp TmGt)    <$ symbol ">")  E.AssocNone],

                [E.Infix (TmBinOp (TmComp TmGe)    <$ symbol ">=") E.AssocRight,
                 E.Infix (TmBinOp (TmComp TmLe)    <$ symbol "<=") E.AssocRight,
                 E.Infix (TmBinOp (TmComp TmEql)   <$ symbol "==") E.AssocRight,
                 E.Infix (TmBinOp (TmComp TmNeq)   <$ symbol "!=") E.AssocRight],

                [E.Infix (TmBinOp (TmLogic TmAnd)  <$ symbol "&&") E.AssocRight],

                [E.Infix (TmBinOp (TmLogic TmOr)   <$ symbol "||")   E.AssocRight]]

parseTerm :: Parser Tm
parseTerm = try parseApplication
            <|> parseCtx
            <|> parseInteger
            <|> parseString
            <|> parseBoolean
            <|> parseDef
            <|> parseLambda
            <|> parens operationParser
            <|> parseUnit

parseExp :: Parser Tm
parseExp = try parseApplication
           <|> try parseAssign   
           <|> parseLambda
           <|> parseConditional
           <|> operationParser
           <|> parseCtx
           <|> parseUnit
           <|> parseDef
           <|> parseBoolean
           <|> parseInteger
           <|> parseString


symbol :: String -> Parser String
symbol s = try $ lexeme $ do
                u <- many1 (oneOf "<>=+-^%/*!&|")
                guard (s == u)
                return s

parens :: Parser a -> Parser a
parens p = lexeme $ between (char '(') (char ')') p

parseConditional :: Parser Tm
parseConditional = TmIf    <$>  (void (keyword "if")       *> parseExp)
                            <*> (void (keyword "then")     *> parseExp)
                            <*> (void (keyword "else")     *> parseExp)

parseMultExpr :: Parser [Tm]
parseMultExpr = parseExp `sepEndBy1` lexeme (char ';')

parseMain :: String -> Either ParseError Tm
parseMain input = case parseWithWhitespace parseMultExpr input of
                        Left err          ->    Left err
                        Right res        ->     Right (merges res)

parseFile :: String -> IO ()
parseFile filePath = do
    content <- catchIOError (readFile filePath) handleError
    case parseMain content of
        Left    err -> putStrLn $ "Parse Error: " ++ show err
        Right   tm  -> print tm

handleError :: IOError -> IO String
handleError _ = return "Error: Unable to read file."