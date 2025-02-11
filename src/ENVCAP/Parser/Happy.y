{
{-# OPTIONS_GHC -Werror=non-exhaustive-patterns #-}
module ENVCAP.Parser.Happy where
import Data.Char
import ENVCAP.Syntax 
}

%name sourceParser
%tokentype     { Token        }
%error         { parseError   }

%token
     int            { TokenInt $$       }
     var            { TokenVar $$       }
     'Sig'          { TokenSig          }          
     'Int'          { TokenTypeInt      }
     'Bool'         { TokenTypeBool     }
     'String'       { TokenTypeString   }
     '->'           { TokenTypeArrow    }
     '&'            { TokenTypeAnd      }
     'False'        { TokenFalse        }
     'True'         { TokenTrue         }
     'if'           { TokenIf           }
     'type'         { TokenTyAlias      }
     'then'         { TokenThen         }
     'else'         { TokenElse         }
     'val'          { TokenValDef       }
     'let'          { TokenLet          }
     'letrec'       { TokenLetrec       }
     'in'           { TokenIn           }
     'function'     { TokenFunc         }
     'struct'       { TokenStruct       }
     'module'       { TokenModule       }
     '[]'           { TokenEmptyList    }
     '['            { TokenOpenSqBracket }
     ']'            { TokenCloseSqBracket }
     '::'           { TokenCons         }
     ','            { TokenComma        }
     '+'            { TokenPlus         }
     '-'            { TokenMinus        }
     '*'            { TokenTimes        }
     '/'            { TokenDiv          }
     '%'            { TokenMod          }
     '('            { TokenOB           }
     ')'            { TokenCB           }
     '?'            { TokenQuery        }
     '>='           { TokenGe           }
     '>'            { TokenGt           }
     '=='           { TokenEql          }
     '!='           { TokenNeq          }
     '<'            { TokenLt           }
     '<='           { TokenLe           }
     '&&'           { TokenAnd          }
     '||'           { TokenOr           }
     ';;'           { TokenSemicolon    }
     ':'            { TokenColon        }
     '='            { TokenEq           }
     '{'            { TokenOpenBracket  }
     '\''           { TokenSingleQuote  }
     '"'            { TokenDoubleQuote  }
     '}'            { TokenCloseBracket }
     '\\('          { TokenLambda       }
     '=>'           { TokenArrow        }

%right '='
%right '=>'
%right '::'
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

Program        : Statements                   { $1 }

Statements     : Statement ';;' Statements    { SMrg $1 $3 }
               | Statement                    { $1 }

Statement      : Function                     { $1 }
               | Module                       { $1 }
               | Binding                      { $1 }
               | Term                         { $1 }

Term           : '?'                               { SCtx }
               | Application                       { $1 }
               | Bool                              { $1 }
               | String                            { $1} 
               | int                               { SLit $1 }
               | var                               { SRProj SCtx $1 }
               | ArithmeticOp                      { $1 }
               | ComparisonOp                      { $1 }
               | BooleanOp                         { $1 }
               | IfThenElse                        { $1 }
               | TyAlias                           { $1 }
               | Lambda                            { $1 }
               | Struct                            { $1 }
               | Let                               { $1 }
               | Letrec                            { $1 }
               | List                              { $1 }
               | ListCons                          { $1 }
               | Record                            { $1 }
               | Tuple                             { $1 }
               | Parens                            { $1 }
               | error                             { parseError [$1] }

Type           : 'Int'                             { STInt }
               | 'Bool'                            { STBool }
               | 'String'                          { STString }
               | Type '->' Type                    { STArrow $1 $3 }
               | Type '&'  Type                    { STAnd $1 $3 }
               | '[' Type ']'                      { STList $2 }
               | '{' RecordType '}'                { $2 }
               | var                               { STIden $1 }
               | Signature                         { $1 }
               | '(' Type ')'                      { $2 }

Signature      : 'Sig' '[' Type ',' Type ']'       { STSig $3 $5 }

Application    : FunctionApplication               %prec application_prec { $1 }

FunctionApplication : var '(' Arguments ')'        { foldl SApp (SRProj SCtx $1) $3 }

Bool           : 'False'                                { SBool False }
               | 'True'                                 { SBool True }

Function       : 'function' var '(' ParamList ')' '{' Term '}'        { SFunc $2 $4 $7 }

Lambda         : '(' Lambda ')' '(' Arguments ')'                     { foldl SApp $2 $5 }
               | '\\(' ParamList ')' '=>' '{' Statements '}'          { SLam $2 $6 }

Binding        : 'val' var '=' Term                                   { SRec $2 $4 }

Module         : 'module' var '(' ParamList ')' '{' Statements '}'    { SModule $2 $4 $7 }
Struct         : 'struct'     '(' ParamList ')' '{' Statements '}'    { SStruct $3 $6 }

String         : '\'' var '\''                         { SString $2 }
               | '"' var '"'                           { SString $2 }

ListCons       : Term '::' Term                        { SCons $1 $3 }

TyAlias        : 'type' var '=' Type                   { SAliasTyp $2 $4 }

Tuple          : '(' TupleElements ')'            { $2 }
TupleElements  : Term ',' TupleElements           { SPair $1 $3 }
               | Term                             { $1 }

RecordType     : Param ',' RecordType             { STAnd $1 $3 }
               | Param                            { $1 }

Record         : '{' Records '}'                  { $2 }
Records        : var '=' Term ',' Records         { SMrg (SRec $1 $3) $5 }
               | var '=' Term                     { SRec $1 $3 }

ParamList      : Param ',' ParamList              { STAnd $1 $3 }
               | Param                            { $1 }

Param          : var ':' Type                     { STRecord $1 $3 }   

Let            : 'let'    var ':' Type '='   Term   'in' CurlyParens   { SLet    $2 $4 $6 $8 }
Letrec         : 'letrec' var ':' Type '='   Term   'in' CurlyParens   { SLetrec $2 $4 $6 $8 }

List           : '[]' ':' Type                    { SNil $3 }
               | '[' Elements ']'                 { $2 }

Elements       : Term ',' Elements                { SCons $1 $3 }
               | Term                             { $1 }

Arguments      : Term ',' Arguments               { $1 : $3 }
               | Term                             { [$1] }

Parens      : '(' Term ')'                        { $2 }

CurlyParens : '{' Statements '}'                  { $2 }

IfThenElse : 'if' Parens 'then' CurlyParens 'else' CurlyParens { SIf $2 $4 $6 }

ComparisonOp   :  Term    '>='    Term                 { SBinOp (Comp  Ge)   $1 $3 }
               |  Term    '>'     Term                 { SBinOp (Comp  Gt)   $1 $3 }
               |  Term    '=='    Term                 { SBinOp (Comp  Eql)  $1 $3 }
               |  Term    '!='    Term                 { SBinOp (Comp  Neq)  $1 $3 }
               |  Term    '<'     Term                 { SBinOp (Comp  Lt)   $1 $3 }
               |  Term    '<='    Term                 { SBinOp (Comp  Le)   $1 $3 }

BooleanOp      : Term    '&&'    Term                  { SBinOp (Logic And)  $1 $3 }
               | Term    '||'    Term                  { SBinOp (Logic Or)   $1 $3 }
          
ArithmeticOp   :    Term    '+'     Term               { SBinOp (Arith Add)  $1 $3 }
               |    Term    '-'     Term               { SBinOp (Arith Sub)  $1 $3 }
               |    Term    '*'     Term               { SBinOp (Arith Mul)  $1 $3 }
               |    Term    '/'     Term               { SBinOp (Arith Div)  $1 $3 }
               |    Term    '%'     Term               { SBinOp (Arith Mod)  $1 $3 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token =   TokenInt Integer       -- Lit i
          |    TokenVar String        -- x
          |    TokenLambda            -- '\'
          |    TokenArrow             -- = >
          |    TokenPlus              -- '+'
          |    TokenMinus             -- '-'
          |    TokenTimes             -- '*'
          |    TokenDiv               -- '/'
          |    TokenMod               -- '%'
          |    TokenEql               -- '=='
          |    TokenNeq               -- '!='
          |    TokenGe                -- '>='
          |    TokenGt                -- '>'
          |    TokenLe                -- '<='
          |    TokenLt                -- '<'
          |    TokenAnd               -- '&&'
          |    TokenOr                -- '||'
          |    TokenOB                -- '('
          |    TokenCB                -- ')'
          |    TokenQuery             -- '?'
          |    TokenEq                -- '='
          |    TokenSemicolon         -- ';;'
          |    TokenIf                -- 'if'
          |    TokenThen              -- 'then'
          |    TokenElse              -- 'else'
          |    TokenValDef            -- 'val'
          |    TokenTrue              -- 'True'
          |    TokenFalse             -- 'False'
          |    TokenFunc              -- 'function'
          |    TokenModule            -- 'module'
          |    TokenStruct            -- 'struct'
          |    TokenOpenBracket       -- '{'
          |    TokenCloseBracket      -- '}'
          |    TokenColon             -- ':'
          |    TokenLet               -- 'let'
          |    TokenLetrec            -- 'letrec'
          |    TokenIn                -- 'in'
          |    TokenSig               -- 'Sig'
          |    TokenTypeInt           -- 'Int'
          |    TokenTypeBool          -- 'Bool'
          |    TokenTypeString        -- 'String'
          |    TokenTypeArrow         -- '->'
          |    TokenTypeAnd           -- '&'
          |    TokenTyAlias           -- 'type'
          |    TokenComma             -- ','
          |    TokenOpenSqBracket     -- '['
          |    TokenCloseSqBracket    -- ']'
          |    TokenCons              -- '::'
          |    TokenEmptyList         -- '[]'
          |    TokenSingleQuote       -- '
          |    TokenDoubleQuote       -- "
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
lexer ('=':cs)      = case cs of
                         ('>':cs') -> TokenArrow  : lexer cs'
                         ('=':cs') -> TokenEql    : lexer cs'
                         _         -> TokenEq     : lexer cs
lexer ('!':cs)      = case cs of
                         ('=':cs') -> TokenNeq : lexer cs'
lexer ('&':cs)      = case cs of
                         ('&':cs') -> TokenAnd         : lexer cs'
                         _         -> TokenTypeAnd     : lexer cs
lexer ('|':cs)      = case cs of
                         ('|':cs') -> TokenOr  : lexer cs'
lexer ('>':cs)      = case cs of
                         ('=':cs') -> TokenGe  : lexer cs'
                         _         -> TokenGt  : lexer cs
lexer ('<':cs)      = case cs of
                         ('=':cs') -> TokenLe  : lexer cs'
                         _         -> TokenLt  : lexer cs
lexer (';':cs)      = case cs of
                         (';':cs') -> TokenSemicolon : lexer cs'
lexer (',':cs)      = TokenComma : lexer cs
lexer (':':cs)      = case cs of 
                         (':':cs')      ->   TokenCons      : lexer cs'
                         _              ->   TokenColon     : lexer cs 
lexer ('{':cs)      = TokenOpenBracket : lexer cs
lexer ('}':cs)      = TokenCloseBracket : lexer cs
lexer ('[':cs)      = case cs of
                         (']':cs') ->   TokenEmptyList     : lexer cs' 
                         _         ->   TokenOpenSqBracket : lexer cs
lexer (']':cs)      = TokenCloseSqBracket : lexer cs
lexer ('(':cs)      = TokenOB       : lexer cs
lexer (')':cs)      = TokenCB       : lexer cs
lexer ('\'':cs)     = TokenSingleQuote : lexer cs
lexer ('"':cs)      = TokenDoubleQuote : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
                    where (num, rest) = span isDigit cs
lexVar cs = case span isAlpha cs of
               ("Int",        rest)     -> TokenTypeInt     : lexer rest
               ("Bool",       rest)     -> TokenTypeBool    : lexer rest
               ("Sig",        rest)     -> TokenSig         : lexer rest
               ("String",     rest)     -> TokenTypeString  : lexer rest
               ("True",       rest)     -> TokenTrue        : lexer rest
               ("False",      rest)     -> TokenFalse       : lexer rest
               ("let",        rest)     -> TokenLet         : lexer rest
               ("letrec",     rest)     -> TokenLetrec      : lexer rest
               ("in",         rest)     -> TokenIn          : lexer rest
               ("type",       rest)     -> TokenTyAlias     : lexer rest
               ("function",   rest)     -> TokenFunc        : lexer rest
               ("module",     rest)     -> TokenModule      : lexer rest
               ("struct",     rest)     -> TokenStruct      : lexer rest
               ("val",        rest)     -> TokenValDef      : lexer rest
               ("if",         rest)     -> TokenIf          : lexer rest
               ("then",       rest)     -> TokenThen        : lexer rest
               ("else",       rest)     -> TokenElse        : lexer rest
               (var,          rest)     -> TokenVar var     : lexer rest

parseSource :: String -> Maybe SurfaceTm
parseSource input = case sourceParser (lexer input) of
                         result -> Just result
                         _      -> Nothing                    

test_cases :: [(String, SurfaceTm)]
test_cases = [  ("?", SCtx)
              , ("1 + 2", SBinOp (Arith Add) (SLit 1) (SLit 2))
              , ("1 - 2", SBinOp (Arith Sub) (SLit 1) (SLit 2))
              , ("3 * 4", SBinOp (Arith Mul) (SLit 3) (SLit 4))
              , ("10 / 2", SBinOp (Arith Div) (SLit 10) (SLit 2))
              , ("5 % 2", SBinOp (Arith Mod) (SLit 5) (SLit 2))
              , ("1 < 2", SBinOp (Comp Lt) (SLit 1) (SLit 2))
              , ("2 <= 3", SBinOp (Comp Le) (SLit 2) (SLit 3))
              , ("3 > 2", SBinOp (Comp Gt) (SLit 3) (SLit 2))
              , ("4 >= 1", SBinOp (Comp Ge) (SLit 4) (SLit 1))
              , ("1 == 1", SBinOp (Comp Eql) (SLit 1) (SLit 1))
              , ("2 != 3", SBinOp (Comp Neq) (SLit 2) (SLit 3))
              , ("a + b * c", SBinOp (Arith Add) (SRProj SCtx "a") (SBinOp (Arith Mul) (SRProj SCtx "b") (SRProj SCtx "c")))
              , ("1 + 2 * c", SBinOp (Arith Add) (SLit 1) (SBinOp (Arith Mul) (SLit 2) (SRProj SCtx "c")))
              , ("((3 + 4) * 2) - (5 / 2) >= (1 + x)", SBinOp   (Comp Ge)
                                                            (SBinOp (Arith Sub)
                                                                 (SBinOp (Arith Mul)
                                                                      (SBinOp (Arith Add) (SLit 3) (SLit 4))
                                                                      (SLit 2))
                                                                 (SBinOp (Arith Div) (SLit 5) (SLit 2)))
                                                            (SBinOp (Arith Add) (SLit 1) (SRProj SCtx "x")))
              , ("(x * 2) + (y / 4) >= 3", SBinOp (Comp Ge)
                                             (SBinOp (Arith Add)
                                                  (SBinOp (Arith Mul) (SRProj SCtx "x") (SLit 2))
                                                  (SBinOp (Arith Div) (SRProj SCtx "y") (SLit 4)))
                                             (SLit 3))
              , ("1 ;; (x * 2) + (y / 4) >= 3", SMrg (SLit 1) (SBinOp (Comp Ge)
                                                                 (SBinOp (Arith Add)
                                                                      (SBinOp (Arith Mul) (SRProj SCtx "x") (SLit 2))
                                                                      (SBinOp (Arith Div) (SRProj SCtx "y") (SLit 4)))
                                                                 (SLit 3)))]

tester :: [(String, SurfaceTm)] -> [Bool]
tester = foldr (\ x -> (++) ([parseSource (fst x) == Just (snd x)])) []

quit :: IO ()
quit = print "runCalc failed\n"

}