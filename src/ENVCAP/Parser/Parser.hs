module ENVCAP.Parser.Parser where
import System.IO.Error (catchIOError)
import ENVCAP.Syntax
import Text.Parsec (ParseError, many1, string, try, between, Parsec, sepEndBy1)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, oneOf, digit)
import Text.Parsec.Combinator (option)
import Control.Applicative ((<|>))
import Control.Monad (void, guard)
import ENVCAP.Parser.Util
    ( falseToken,
      identifierToken,
      intersections,
      keyword,
      lexeme,
      merges,
      parseWithWhitespace,
      stringToken,
      trueToken,
      unitToken )
import Text.Parsec.Expr as E (buildExpressionParser, Assoc(AssocNone), Assoc(AssocLeft), Assoc(AssocRight), Operator(Infix, Prefix) )
import Data.Functor.Identity (Identity)


parseCtx        :: Parser SourceTm
parseCtx        = lexeme $ keyword "?" >> return TmCtx

parseTrue       :: Parser SourceTm
parseTrue       = lexeme $ trueToken    >> return (TmBool True)

parseFalse      :: Parser SourceTm
parseFalse      = lexeme $ falseToken   >> return (TmBool False)

parseBoolean    :: Parser SourceTm
parseBoolean    = try parseTrue <|> parseFalse

parseString     :: Parser SourceTm
parseString     = TmString <$> lexeme stringToken

parseInteger :: Parser SourceTm
parseInteger = lexeme $ do
                        sign    <-  option "" (string "-")      -- Handling Prefix -
                        void    $   option "" (string "+")      -- Handling Prefix +
                        num     <-  read <$> many1 digit
                        return $ TmLit (if null sign then num else -num)

parseUnit   :: Parser SourceTm
parseUnit   = lexeme    $ void unitToken >> return TmUnit

parseDef    :: Parser SourceTm
parseDef    = TmRProj TmCtx <$> identifierToken

parseAssign :: Parser SourceTm
parseAssign = do
                void (lexeme $ keyword "define")
                name <- identifierToken
                void (lexeme $ char '=')
                TmRec name <$> parseExp

parseType :: Parser SourceTyp
parseType = do
                void (lexeme $ string "Int")
                return TySInt

parseLambdaParams :: Parser [SourceTyp]
parseLambdaParams = parseParam `sepEndBy1` lexeme (char ',')

parseParam :: Parser SourceTyp
parseParam = do
    name <- identifierToken
    void (lexeme $ char ':')
    TySRecord name <$> parseType


parseLambda :: Parser SourceTm
parseLambda = do
                void (lexeme $ string "\\(")
                ty <- lexeme parseLambdaParams
                void (lexeme $ char ')')
                void (lexeme $ symbol "=>")
                tm <- lexeme $ between (lexeme $ char '{') (lexeme $ char '}') parseMultExpr
                return $ TmLam (intersections ty) (merges tm)

parseApplication :: Parser SourceTm
parseApplication = do
                        funcName <- identifierToken
                        void $ lexeme $ char '('
                        e <- lexeme parseExp
                        void $ lexeme $ char ')'
                        return $ TmApp (TmRProj TmCtx funcName) e


operationParser :: Parsec String () SourceTm
operationParser = lexeme $ buildExpressionParser operators parseTerm

operators :: [[Operator String () Identity SourceTm]]
operators =    [[E.Prefix (TmUnOp Not            <$ char '!')],
                [E.Infix (TmBinOp (Arith Mod)  <$ symbol "%") E.AssocLeft,
                 E.Infix (TmBinOp (Arith Mul)  <$ symbol "*") E.AssocLeft,
                 E.Infix (TmBinOp (Arith Div)  <$ symbol "/") E.AssocLeft],

                [E.Infix (TmBinOp (Arith Add)  <$ symbol "+") E.AssocLeft,
                 E.Infix (TmBinOp (Arith Sub)  <$ symbol "-") E.AssocLeft],

                [E.Infix (TmBinOp (Comp Lt)    <$ symbol "<")  E.AssocNone,
                 E.Infix (TmBinOp (Comp Gt)    <$ symbol ">")  E.AssocNone],
                [E.Infix (TmBinOp (Comp Ge)    <$ symbol ">=") E.AssocRight,
                 E.Infix (TmBinOp (Comp Le)    <$ symbol "<=") E.AssocRight,
                 E.Infix (TmBinOp (Comp Eql)   <$ symbol "==") E.AssocRight,
                 E.Infix (TmBinOp (Comp Neq)   <$ symbol "!=") E.AssocRight],
                [E.Infix (TmBinOp (Logic And)  <$ symbol "&&") E.AssocRight],
                [E.Infix (TmBinOp (Logic Or)   <$ symbol "||")   E.AssocRight]]

parseTerm :: Parser SourceTm
parseTerm = try parseApplication
            <|> parseCtx
            <|> parseInteger
            <|> parseString
            <|> parseBoolean
            <|> parseDef
            <|> parseLambda
            <|> parens operationParser
            <|> parseUnit

parseExp :: Parser SourceTm
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

parseConditional :: Parser SourceTm
parseConditional = TmIf    <$>  (void (keyword "if")       *> parseExp)
                            <*> (void (keyword "then")     *> parseExp)
                            <*> (void (keyword "else")     *> parseExp)

parseMultExpr :: Parser [SourceTm]
parseMultExpr = parseExp `sepEndBy1` lexeme (char ';')

parseMain :: String -> Either ParseError SourceTm
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