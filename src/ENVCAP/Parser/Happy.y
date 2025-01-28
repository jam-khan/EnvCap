{
{-# OPTIONS_GHC -Werror=non-exhaustive-patterns #-}
module ENVCAP.Parser.Happy where
import Data.Char
import ENVCAP.Source.Syntax 
}

%name sourceParser
%tokentype { Token }
%error { parseError }

%token
     int            { TokenInt $$ }
     var            { TokenVar $$ }
     'if'           { TokenIf }
     'type'         { TokenTyAlias }
     'then'         { TokenThen }
     'else'         { TokenElse }
     'def'          { TokenDefine }
     'False'        { TokenFalse }
     'True'         { TokenTrue }
     'function'     { TokenFunc }
     'Int'          { TokenTypeInt }
     'Bool'         { TokenTypeBool }
     'String'       { TokenTypeString }
     '->'           { TokenTypeArrow }
     '&'            { TokenTypeAnd }
     '['            { TokenOpenSqBracket }
     ']'            { TokenCloseSqBracket }
     ','            { TokenComma }
     '+'            { TokenPlus }
     '-'            { TokenMinus }
     '*'            { TokenTimes }
     '/'            { TokenDiv }
     '%'            { TokenMod }
     '('            { TokenOB }
     ')'            { TokenCB }
     '?'            { TokenQuery }
     '>='           { TokenGe }
     '>'            { TokenGt }
     '=='           { TokenEql }
     '!='           { TokenNeq }
     '<'            { TokenLt }
     '<='           { TokenLe }
     '&&'           { TokenAnd }
     '||'           { TokenOr }
     ';;'           { TokenSemicolon }
     ':'            { TokenColon }
     '='            { TokenEq }
     '{'            { TokenOpenBracket }
     '}'            { TokenCloseBracket }
     '\\('           { TokenLambda }
     '=>'           { TokenArrow }


%right '='
%right '=>'
%left TokenElse
%left application_prec
%left '->'
%left '&'
%left '||'
%left '&&'
%left '>=' '>' '==' '!=' '<' '<='
%left '+' '-'
%left '*' '/' '%'


%%
Program   : Statements                          { $1 }
          
Statements     : Term ';;' Statements            { TmMrg $1 $3 }
               | Term                           { $1 }
          
Term      : '?'                               { TmCtx }
          | Function                          { $1 }
          | Application                       { $1 }
          | Bool                              { $1 }
          | int                               { TmLit $1 }
          | var                               { TmRProj TmCtx $1 }
          | ArithmeticOp                      { $1 }
          | ComparisonOp                      { $1 }
          | BooleanOp                         { $1 }
          | Binding                           { $1 }
          | IfThenElse                        { $1 }
          | TyAlias                           { $1 }
          | Lambda                            { $1 }
          | Parens                            { $1 }
          | error                             { parseError [$1] }

TyAlias   : 'type' var '=' Type               { TmAliasTyp $2 $4 }

Type      : 'Int'                             { TInt }
          | 'Bool'                            { TBool }
          | 'String'                          { TString }
          | Type '->' Type                    { TArrow $1 $3 }
          | Type '&'  Type                    { TAnd $1 $3 }
          | '[' Type ']'                      { TList $2 }
          | '{' var ':' Type '}'              { TRecord $2 $4 }
          | var                               { TIden $1 }
          
ParamList : Param ',' ParamList               { TAnd $1 $3 }
          | Param                             { $1 }

Param     : var ':' Type                      { TRecord $1 $3 }   


ComparisonOp   :  Term    '>='    Term                 { TmBinOp (TmComp  TmGe)   $1 $3 }
               |  Term    '>'     Term                 { TmBinOp (TmComp  TmGt)   $1 $3 }
               |  Term    '=='    Term                 { TmBinOp (TmComp  TmEql)  $1 $3 }
               |  Term    '!='    Term                 { TmBinOp (TmComp  TmNeq)  $1 $3 }
               |  Term    '<'     Term                 { TmBinOp (TmComp  TmLt)   $1 $3 }
               |  Term    '<='    Term                 { TmBinOp (TmComp  TmLe)   $1 $3 }

BooleanOp      : Term    '&&'    Term                  { TmBinOp (TmLogic TmAnd)  $1 $3 }
               | Term    '||'    Term                  { TmBinOp (TmLogic TmOr)   $1 $3 }
          
ArithmeticOp   :    Term    '+'     Term               { TmBinOp (TmArith TmAdd)  $1 $3 }
               |    Term    '-'     Term               { TmBinOp (TmArith TmSub)  $1 $3 }
               |    Term    '*'     Term               { TmBinOp (TmArith TmMul)  $1 $3 }
               |    Term    '/'     Term               { TmBinOp (TmArith TmDiv)  $1 $3 }
               |    Term    '%'     Term               { TmBinOp (TmArith TmMod)  $1 $3 }

Arguments      : Term ',' Arguments                         { $1 : $3 }
               | Term                                       { [$1] }

Application    : FunctionApplication                   %prec application_prec { $1 }
               

FunctionApplication : var '(' Arguments ')'            { foldl TmApp (TmRProj TmCtx $1) $3 }

Bool      : 'False'                                    { TmBool False }
          | 'True'                                     { TmBool True }

Function  : 'function' var '(' ParamList ')' '{' Statements '}'            { TmFunc $2 $4 $7 }

Lambda    : '(' Lambda ')' '(' Arguments ')'              { foldl TmApp $2 $5 }
          | '\\(' ParamList ')' '=>' '{' Statements '}'   { TmLam $2 $6 }           

Binding   : 'def' var '=' Term                         { TmRec $2 $4 }

Parens      : '(' Term ')'                             { $2 }
CurlyParens : '{' Statements '}'                       { $2 }

IfThenElse : 'if' Parens 'then' CurlyParens 'else' CurlyParens { TmIf $2 $4 $6 }
           | 'if' Parens 'then' CurlyParens                    { TmIf $2 $4 TmUnit }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
     = TokenInt Integer  -- Lit i
     | TokenVar String   -- x
     | TokenLambda       -- '\'
     | TokenArrow        -- = >
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
     | TokenSemicolon    -- ';'
     | TokenIf           -- 'if'
     | TokenThen         -- 'then'
     | TokenElse         -- 'else'
     | TokenDefine       -- 'def'
     | TokenTrue         -- 'True'
     | TokenFalse        -- 'False'
     | TokenFunc         -- 'function'
     | TokenOpenBracket  -- '{'
     | TokenCloseBracket -- '}'
     | TokenColon        -- ':'
     | TokenTypeInt      -- 'Int'
     | TokenTypeBool     -- 'Bool'
     | TokenTypeString   -- 'String'
     | TokenTypeArrow    -- '->'
     | TokenTypeAnd      -- '&'
     | TokenTyAlias      -- 'type'
     | TokenComma        -- ','
     | TokenOpenSqBracket  -- '['
     | TokenCloseSqBracket -- ']'
     deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexVar (c:cs)
     | isDigit c = lexNum (c:cs)
lexer ('?':cs)      = TokenQuery    : lexer cs
lexer ('+':cs)      = TokenPlus     : lexer cs
lexer ('-':cs)      = 
     case cs of
          ('>':cs')      -> TokenTypeArrow   : lexer cs'
          _              -> TokenMinus       : lexer cs
lexer ('*':cs)      = TokenTimes    : lexer cs
lexer ('/':cs)      = TokenDiv      : lexer cs
lexer ('%':cs)      = TokenMod      : lexer cs
lexer ('\\':cs)     = case cs of 
                         ('(':cs') -> TokenLambda   : lexer cs' 
lexer ('=':cs)      = 
     case cs of
          ('>':cs') -> TokenArrow  : lexer cs'
          ('=':cs') -> TokenEql    : lexer cs'
          _         -> TokenEq     : lexer cs
lexer ('!':cs) = 
     case cs of
          ('=':cs') -> TokenNeq : lexer cs'
lexer ('&':cs) =
     case cs of
          ('&':cs') -> TokenAnd         : lexer cs'
          _         -> TokenTypeAnd     : lexer cs
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
lexer (';':cs)      = case cs of
                         (';':cs') -> TokenSemicolon : lexer cs'
lexer (',':cs)      = TokenComma : lexer cs
lexer (':':cs)      = TokenColon : lexer cs
lexer ('{':cs)      = TokenOpenBracket : lexer cs
lexer ('}':cs)      = TokenCloseBracket : lexer cs
lexer ('[':cs)      = TokenOpenSqBracket : lexer cs
lexer (']':cs)      = TokenCloseSqBracket : lexer cs
lexer ('(':cs)      = TokenOB       : lexer cs
lexer (')':cs)      = TokenCB       : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
     where (num, rest) = span isDigit cs

lexVar cs = 
     case span isAlpha cs of
          ("Int",        rest)     -> TokenTypeInt     : lexer rest
          ("Bool",       rest)     -> TokenTypeBool    : lexer rest
          ("String",     rest)     -> TokenTypeString  : lexer rest
          ("True",       rest)     -> TokenTrue        : lexer rest
          ("False",      rest)     -> TokenFalse       : lexer rest
          ("type",       rest)     -> TokenTyAlias     : lexer rest
          ("function",   rest)     -> TokenFunc        : lexer rest
          ("def",        rest)     -> TokenDefine      : lexer rest
          ("if",         rest)     -> TokenIf          : lexer rest
          ("then",       rest)     -> TokenThen        : lexer rest
          ("else",       rest)     -> TokenElse        : lexer rest
          (var,          rest)     -> TokenVar var     : lexer rest

parseSource :: String -> Maybe Tm
parseSource input = case sourceParser (lexer input) of
                         result -> Just result
                         _      -> Nothing                    

test_cases :: [(String, Tm)]
test_cases = [  ("?", TmCtx)
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
              , ("a + b * c", TmBinOp (TmArith TmAdd) (TmRProj TmCtx "a") (TmBinOp (TmArith TmMul) (TmRProj TmCtx "b") (TmRProj TmCtx "c")))
              , ("1 + 2 * c", TmBinOp (TmArith TmAdd) (TmLit 1) (TmBinOp (TmArith TmMul) (TmLit 2) (TmRProj TmCtx "c")))
              , ("((3 + 4) * 2) - (5 / 2) >= (1 + x)", TmBinOp   (TmComp TmGe)
                                                            (TmBinOp (TmArith TmSub)
                                                                 (TmBinOp (TmArith TmMul)
                                                                      (TmBinOp (TmArith TmAdd) (TmLit 3) (TmLit 4))
                                                                      (TmLit 2))
                                                                 (TmBinOp (TmArith TmDiv) (TmLit 5) (TmLit 2)))
                                                            (TmBinOp (TmArith TmAdd) (TmLit 1) (TmRProj TmCtx "x")))
              , ("(x * 2) + (y / 4) >= 3", TmBinOp (TmComp TmGe)
                                             (TmBinOp (TmArith TmAdd)
                                                  (TmBinOp (TmArith TmMul) (TmRProj TmCtx "x") (TmLit 2))
                                                  (TmBinOp (TmArith TmDiv) (TmRProj TmCtx "y") (TmLit 4)))
                                             (TmLit 3))
              , ("1 ;; (x * 2) + (y / 4) >= 3", TmMrg (TmLit 1) (TmBinOp (TmComp TmGe)
                                                                 (TmBinOp (TmArith TmAdd)
                                                                      (TmBinOp (TmArith TmMul) (TmRProj TmCtx "x") (TmLit 2))
                                                                      (TmBinOp (TmArith TmDiv) (TmRProj TmCtx "y") (TmLit 4)))
                                                                 (TmLit 3)))]

tester :: [(String, Tm)] -> [Bool]
tester = foldr (\ x -> (++) ([parseSource (fst x) == Just (snd x)])) []

quit :: IO ()
quit = print "runCalc failed\n"

}
