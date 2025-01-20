{
module ENVCAP.Parser.Happy where
import Data.Char
import ENVCAP.Source.Syntax
}

%name calc
%tokentype { Token }
%error { parseError }

%token
     int       { TokenInt $$ }
     var       { TokenVar $$ }
     '+'       { TokenPlus }
     '-'       { TokenMinus }
     '*'       { TokenTimes }
     '/'       { TokenDiv }
     '%'       { TokenMod }
     '('       { TokenOB }
     ')'       { TokenCB }
     '?'       { TokenQuery }
     '>='      { TokenGe }
     '>'       { TokenGt }
     '=='      { TokenEql }
     '!='      { TokenNeq }
     '<'       { TokenLt }
     '<='      { TokenLe }
     '&&'      { TokenAnd }
     '||'      { TokenOr }

%left '||'
%left '&&'
%left '>='
%left '>'
%left '=='
%left '!='
%left '<'
%left '<='
%left '+'
%left '-'
%left '*'
%left '/'
%left '%'

%%

Term      : '?'                               { TmCtx }
          | Term    '+'     Term              { TmBinOp (TmArith TmAdd)  $1 $3 }
          | Term    '-'     Term              { TmBinOp (TmArith TmSub)  $1 $3 }
          | Term    '*'     Term              { TmBinOp (TmArith TmMul)  $1 $3 }
          | Term    '/'     Term              { TmBinOp (TmArith TmDiv)  $1 $3 }
          | Term    '%'     Term              { TmBinOp (TmArith TmMod)  $1 $3 }
          | Term    '>='    Term              { TmBinOp (TmComp  TmGe)   $1 $3 }
          | Term    '>'     Term              { TmBinOp (TmComp  TmGt)   $1 $3 }
          | Term    '=='    Term              { TmBinOp (TmComp  TmEql)  $1 $3 }
          | Term    '!='    Term              { TmBinOp (TmComp  TmNeq)  $1 $3 }
          | Term    '<'     Term              { TmBinOp (TmComp  TmLt)   $1 $3 }
          | Term    '<='    Term              { TmBinOp (TmComp  TmLe)   $1 $3 }
          | Term    '&&'    Term              { TmBinOp (TmLogic TmAnd)  $1 $3 }
          | Term    '||'    Term              { TmBinOp (TmLogic TmOr)   $1 $3 }
          | int                               { TmLit $1 }
          | var                               { TmVar $1 }
          | '(' Term ')'                      { $2 } 

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
     = TokenInt Integer  -- Lit i
     | TokenVar String   -- x
     | TokenPlus         -- '+'
     | TokenMinus        -- '-'
     | TokenTimes        -- '*'
     | TokenDiv          -- '/'
     | TokenMod          -- '%'
     | TokenEql          -- '=='
     | TokenNeq          -- '!='
     | TokenGe           -- '>='
     | TokenGt           -- '>'
     | TokenLe           -- '<='
     | TokenLt           -- '<'
     | TokenAnd          -- '&&'
     | TokenOr           -- '||'
     | TokenOB           -- '('
     | TokenCB           -- ')'
     | TokenQuery        -- '?'
     | TokenEq           -- '='
     deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexVar (c:cs)
     | isDigit c = lexNum (c:cs)
lexer ('?':cs)      = TokenQuery    : lexer cs
lexer ('+':cs)      = TokenPlus     : lexer cs
lexer ('-':cs)      = TokenMinus    : lexer cs
lexer ('*':cs)      = TokenTimes    : lexer cs
lexer ('/':cs)      = TokenDiv      : lexer cs
lexer ('%':cs)      = TokenMod      : lexer cs
lexer ('(':cs)      = TokenOB       : lexer cs
lexer (')':cs)      = TokenCB       : lexer cs
lexer ('=':cs)      = 
     case cs of
          ('=':cs') -> TokenEql : lexer cs'
          _         -> TokenEq  : lexer cs
lexer ('!':cs) = 
     case cs of
          ('=':cs') -> TokenNeq : lexer cs'
lexer ('&':cs) =
     case cs of
          ('&':cs') -> TokenAnd : lexer cs'
lexer ('|':cs) =
     case cs of
          ('|':cs') -> TokenOr  : lexer cs'
lexer ('>':cs) =
     case cs of
          ('=':cs') -> TokenGe  : lexer cs'
          _         -> TokenGt  : lexer cs
lexer ('<':cs) =
     case cs of
          ('=':cs') -> TokenLe  : lexer cs'
          _         -> TokenLt  : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
     where (num, rest) = span isDigit cs

lexVar cs = 
     case span isAlpha cs of
          (var,   rest) -> TokenVar var : lexer rest

runCalc :: String -> Tm
runCalc = calc . lexer

test_cases :: [(String, Tm)]
test_cases = [ ("?", TmCtx)
              , ("1 + 2", TmBinOp (TmArith TmAdd) (TmLit 1) (TmLit 2))
              , ("1 - 2", TmBinOp (TmArith TmSub) (TmLit 1) (TmLit 2))
              , ("3 * 4", TmBinOp (TmArith TmMul) (TmLit 3) (TmLit 4))
              , ("10 / 2", TmBinOp (TmArith TmDiv) (TmLit 10) (TmLit 2))
              , ("5 % 2", TmBinOp (TmArith TmMod) (TmLit 5) (TmLit 2))
              , ("1 < 2", TmBinOp (TmComp TmLt) (TmLit 1) (TmLit 2))
              , ("2 <= 3", TmBinOp (TmComp TmLe) (TmLit 2) (TmLit 3))
              , ("3 > 2", TmBinOp (TmComp TmGt) (TmLit 3) (TmLit 2))
              , ("4 >= 1", TmBinOp (TmComp TmGe) (TmLit 4) (TmLit 1))
              , ("1 == 1", TmBinOp (TmComp TmEql) (TmLit 1) (TmLit 1))
              , ("2 != 3", TmBinOp (TmComp TmNeq) (TmLit 2) (TmLit 3))
              , ("a + b * c", TmBinOp (TmArith TmAdd) (TmVar "a") (TmBinOp (TmArith TmMul) (TmVar "b") (TmVar "c")))
              , ("1 + 2 * c", TmBinOp (TmArith TmAdd) (TmLit 1) (TmBinOp (TmArith TmMul) (TmLit 2) (TmVar "c")))
              , ("((3 + 4) * 2) - (5 / 2) >= (1 + x)", TmBinOp   (TmComp TmGe)
                                                            (TmBinOp (TmArith TmSub)
                                                                 (TmBinOp (TmArith TmMul)
                                                                      (TmBinOp (TmArith TmAdd) (TmLit 3) (TmLit 4))
                                                                      (TmLit 2))
                                                                 (TmBinOp (TmArith TmDiv) (TmLit 5) (TmLit 2)))
                                                            (TmBinOp (TmArith TmAdd) (TmLit 1) (TmVar "x")))
              , ("(x * 2) + (y / 4) >= 3", TmBinOp (TmComp TmGe)
                                             (TmBinOp (TmArith TmAdd)
                                                  (TmBinOp (TmArith TmMul) (TmVar "x") (TmLit 2))
                                                  (TmBinOp (TmArith TmDiv) (TmVar "y") (TmLit 4)))
                                             (TmLit 3))  
          ]

tester :: [(String, Tm)] -> Bool
tester = foldr (\ x -> (&&) (runCalc (fst x) == snd x)) True

quit :: IO ()
quit = print "runCalc failed\n"

}
