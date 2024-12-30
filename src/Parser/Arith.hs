module Parser.Arith where
import Surface.Syntax (Tm(..), Typ(..), TmBinaryOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))
import Parser.Tokens (identifierToken, trueToken, falseToken, contextToken, unitToken, addToken, subToken, multToken, divToken, modToken, andToken, orToken)
import Text.Parsec (ParseError, many1, string, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (satisfy, char, oneOf, digit, letter)
import Text.Parsec.Combinator (eof, manyTill, anyToken, chainl1)
import Data.Char (isLetter, isDigit)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many)
import Control.Monad (void)
import Parser.Util (lexeme)
import Core.Syntax (Exp(BinOp))

-- 


{--
data Tm =   TmCtx                              -- Context
        |   TmUnit                             -- Unit
        |   TmVar       String                 -- Variable
        |   TmInt       Int                    -- Integer Literal
        |   TmBool      Bool                   -- Boolean Literal
        |   TmString    String                 -- String  Literal
        |   TmBinary    TmBinaryOp Tm Tm       -- Binary Operation
        |   TmUnary     TmUnaryOp Tm           -- Unary Operation
        |   TmIf        Tm Tm Tm               -- Conditional
--}

{--
-- Operations Definitions
data TmBinaryOp   =     TmApp             -- Application
                |       TmBox             -- Box
                |       TmMrg             -- Merge
                --      Extensions
                |       TmArith TmArithOp   -- Arithmetic
                |       TmComp  TmCompOp    -- CompOp
                |       TmLogic TmLogicOp   -- Boolean Logic

data TmUnaryOp  =       TmNot
                |       TmIndex Int
        deriving Eq

data TmArithOp   = TmAdd | TmSub | TmMul | TmDiv | TmMod 
        deriving Eq
data TmCompOp    = TmEql | TmNeq | TmLt | TmLe | TmGt | TmGe
        deriving Eq
data TmLogicOp   = TmAnd | TmOr
        deriving Eq
--}

-- It is a bit tricky tho
data ParseBinaryOp = ParseBinaryOp Tm Tm String

-- Parse 

examples :: [(String, Tm)]
examples = [("context()", TmCtx),
            ("1", TmInt 1),
            ("false", TmBool False),
            ("true", TmBool True),
            (" \'hello\' ", TmString "hello"),
            ("1 + 2", TmBinary (TmArith TmAdd) (TmInt 1) (TmInt 2)),
            ("1 - 2", TmBinary (TmArith TmSub) (TmInt 1) (TmInt 2)),
            ("1 * 2", TmBinary (TmArith TmMul) (TmInt 1) (TmInt 2)),
            ("1 / 2", TmBinary (TmArith TmDiv) (TmInt 1) (TmInt 2)),
            ("1 % 2", TmBinary (TmArith TmMod) (TmInt 1) (TmInt 2)),
            ("if true then 1 + 2 else 1 - 2", TmIf  (TmBool True)
                                                    (TmBinary (TmArith TmAdd) (TmInt 1) (TmInt 2))
                                                    (TmBinary (TmArith TmSub) (TmInt 1) (TmInt 2)))]


-- Parser for context

parseCtx :: Parser Tm
parseCtx = lexeme $ contextToken >> return TmCtx

-- Parser for boolean literals

parseTrue :: Parser Tm
parseTrue = lexeme $ trueToken >> return (TmBool True)

parseFalse :: Parser Tm
parseFalse = lexeme $ falseToken >> return (TmBool False)

parseBoolean :: Parser Tm
parseBoolean  = try parseTrue <|> parseFalse


-- Parser for integer literals

parseInteger :: Parser Tm
parseInteger = TmInt . read <$> lexeme (many1 digit)

-- Parser for unit

parseUnit :: Parser Tm
parseUnit = lexeme $ void unitToken >> return TmUnit

-- Parser for variable

parseVar :: Parser Tm
parseVar = TmVar <$> identifierToken

-- Parser for arithmetic operations

parseArith :: Parser Tm
parseArith = chainl1 parseInteger op
  where
    op = lexeme $ addOp <|> subOp <|> multOp <|> divOp <|> modOp

    addOp       = do    void addToken
                        return (TmBinary (TmArith TmAdd))
    subOp       = do    void subToken
                        return (TmBinary (TmArith TmSub))
    multOp      = do    void multToken
                        return (TmBinary (TmArith TmMul))
    divOp       = do    void divToken
                        return (TmBinary (TmArith TmDiv))
    modOp       = do    void modToken
                        return (TmBinary (TmArith TmMod))

-- Parser for logical operations

parseLogic :: Parser Tm
parseLogic = chainl1 parseBoolean logicOp
        where
                logicOp = lexeme $ andOp <|> orOp

                andOp = do
                        void andToken
                        return (TmBinary (TmLogic TmAnd))
                
                orOp = do
                        void orToken
                        return (TmBinary (TmLogic TmOr))
-- parseAdd :: Parser Tm
-- parseAdd = do
--         left <- parseInt
--         lexeme $ void addToken
--         TmBinary (TmArith TmAdd) left <$> parseInt


