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
     '['            { TokenOpenSqBracket  }
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
     'env'          { TokenQuery        }
     '.'            { TokenProjection   }
     '>='           { TokenGe           }
     '>'            { TokenGt           }
     '=='           { TokenEql          }
     '!='           { TokenNeq          }
     '<'            { TokenLt           }
     '<='           { TokenLe           }
     '&&'           { TokenAnd          }
     '||'           { TokenOr           }
     ';'            { TokenSemicolon    }
     ':'            { TokenColon        }
     '='            { TokenEq           }
     '{'            { TokenOpenBracket  }
     '\''           { TokenSingleQuote  }
     '"'            { TokenDoubleQuote  }
     '}'            { TokenCloseBracket }
     '\\('          { TokenLambda       }
     '=>'           { TokenArrow        }
     ',,'           { TokenDepMerge }

%right '='
%right '=>'
%right '::'
%left '.'
%left proj
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

Program        : Statements                       { $1 }

Statements     : Statement ';' Statements         { SMrg $1 $3 }
               | Statement                        { $1 }

Statement      : Function                         { $1 }
               | Module                           { $1 }
               | Binding                          { $1 }
               | TyAlias                          { $1 }
               | Term                             { $1 }

Term           : 'env'                             { SCtx }
               | FunctionApplication               { $1 }
               | Bool                              { $1 }
               | String                            { $1} 
               | Projection            %prec proj  { $1 }
               | int                               { SLit $1 }
               | var                               { SVar $1 } -- Parsed into a general variable that will later get separated into
               | ArithmeticOp                      { $1 }
               | ComparisonOp                      { $1 }
               | BooleanOp                         { $1 }
               | IfThenElse                        { $1 }
               | Lambda                            { $1 }
               | Struct                            { $1 }
               | Let                               { $1 }
               | Letrec                            { $1 }
               | List                              { $1 }
               | ListCons                          { $1 }
               | Record                            { $1 }
               | Tuple                             { $1 }
               | DependentMerge                    { $1 }
               | Parens                            { $1 }
               | error                             { parseError [$1] }

Type           : 'Int'                             { STInt }
               | 'Bool'                            { STBool }
               | 'String'                          { STString }
               | Type '->' Type                    { STArrow $1 $3 }
               | Type '&'  Type                    { STAnd $1 $3 }
               | '[' Type ']'                      { STList $2 }
               | '{' RecordType '}'                { $2 }
               | Signature                         { $1 }
               | var                               { STIden $1 }
               | '(' Type ')'                      { $2 }

Projection     : Term '.' int                     {SProj $1 $3}
               | Term '.' var                     {SRProj $1 $3}

Module         : 'module' var '(' ParamList ')' '{' Statements '}'    { SModule $2 $4 $7 }

Struct         : 'struct'     '(' ParamList ')' '{' Statements '}'    { SStruct $3 $6 }

Signature      : 'Sig' '[' Type ',' Type ']'                          { STSig $3 $5 }

Function       : 'function' var '(' ParamList ')' ':' Type '{' Term '}'        { SFunc $2 $4 $7 $9 }

FunctionApplication : var '(' Arguments ')'  %prec application_prec   { SApp (SVar $1) $3 }

Binding        : 'val' var '=' Term                                   { SRec $2 $4 }

Lambda         : '(' Lambda ')' '(' Arguments ')'                     { SApp $2 $5 }
               | '\\(' ParamList ')' '=>' '{' Statements '}'          { SLam $2 $6 }

Bool           : 'False' { SBool False }
               | 'True'  { SBool True }

String         : '\'' var '\''                         { SString $2 }
               | '"' var '"'                           { SString $2 }

ListCons       : Term '::' Term                        { SCons $1 $3 }

TyAlias        : 'type' var '=' Type                   { SAliasTyp $2 $4 }

-- There must be atleast one element in tuple I guess this satisfies it but
-- it will create ambiguities, so must have atleast two elements

-- It must have atleast two elements
DependentMerge           : '(' DependentMergeElements ')'   { $2 }
DependentMergeElements   : Term ',,' DependentMergeElements { SMrg $1 $3 }
                         | Term ',,' Term                   { SMrg $1 $3 }

Tuple          : '(' TupleElements ')'                 { STuple $2 }

TupleElements  : Term ',' TupleElements                { $1 : $3 }
               | Term ',' Term                         { $1 : [$3] }

RecordType     : Param ',' RecordType                  { STAnd $1 $3 }
               | Param                                 { $1 }

Param          : var ':' Type                          { STRecord $1 $3 }   

Record         : '{' Records '}'                       { $2 }
Records        : '"' var '"' '=' Term ',' Records      { SMrg (SRec $2 $5) $7 }
               | '"' var '"' '=' Term                  { SRec $2 $5 }
               | '\'' var '\'' '=' Term ',' Records    { SMrg (SRec $2 $5) $7 }
               | '\'' var '\'' '=' Term                { SRec $2 $5 }

ParamList      : ParamL ',' ParamList                  { $1 : $3 }
               | ParamL                                { [$1] }

ParamL         : var ':' Type                          { ($1, $3) } 

Let            : 'let'    '{' bindings '}' 'in' '{' Term '}'   { SLet    $3 $7 }
Letrec         : 'letrec' '{' bindings '}' 'in' '{' Term '}'   { SLetrec $3 $7 }

bindings       : binding ';' bindings                  { $1 : $3 }
               | binding                               { [$1] }

binding        : var ':' Type '=' Term                 { ($1, $3, $5) }

List           : '[]' ':' Type                         { SNil $3 }
               | '[' Elements ']'                      { $2 }

Elements       : Term ',' Elements                     { SCons $1 $3 }
               | Term                                  { $1 }

Arguments      : Term ',' Arguments                    { $1 : $3 }
               | Term                                  { [$1] }

Parens      : '(' Term ')'                             { $2 }

CurlyParens : '{' Statements '}'                       { $2 }

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
          |    TokenQuery             -- 'env()'
          |    TokenEq                -- '='
          |    TokenSemicolon         -- ';'
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
          |    TokenDepMerge          -- ,,
          |    TokenProjection
          deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexVar (c:cs)
     | isDigit c = lexNum (c:cs)
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
lexer (';':cs)      = TokenSemicolon : lexer cs
lexer (',':cs)      = case cs of
                         (',':cs')  -> TokenDepMerge : lexer cs'
                         _          -> TokenComma : lexer cs
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
lexer ('.':cs)      = TokenProjection : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
                    where (num, rest) = span isDigit cs
lexVar cs = case span isAlpha cs of
               ("Int",        rest)     -> TokenTypeInt     : lexer rest
               ("Bool",       rest)     -> TokenTypeBool    : lexer rest
               ("Sig",        rest)     -> TokenSig         : lexer rest
               ("String",     rest)     -> TokenTypeString  : lexer rest
               ("True",       rest)     -> TokenTrue        : lexer rest
               ("env",        rest)     -> TokenQuery       : lexer rest
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
test_cases = [  ("env", SCtx)
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
              , ("a + b * c", SBinOp (Arith Add) (SVar "a") (SBinOp (Arith Mul) (SVar "b") (SVar "c")))
              , ("1 + 2 * c", SBinOp (Arith Add) (SLit 1) (SBinOp (Arith Mul) (SLit 2) (SVar "c")))
              , ("((3 + 4) * 2) - (5 / 2) >= (1 + x)", SBinOp   (Comp Ge)
                                                            (SBinOp (Arith Sub)
                                                                 (SBinOp (Arith Mul)
                                                                      (SBinOp (Arith Add) (SLit 3) (SLit 4))
                                                                      (SLit 2))
                                                                 (SBinOp (Arith Div) (SLit 5) (SLit 2)))
                                                            (SBinOp (Arith Add) (SLit 1) (SVar "x")))
              , ("(x * 2) + (y / 4) >= 3", SBinOp (Comp Ge)
                                             (SBinOp (Arith Add)
                                                  (SBinOp (Arith Mul) (SVar "x") (SLit 2))
                                                  (SBinOp (Arith Div) (SVar "y") (SLit 4)))
                                             (SLit 3))
              , ("1 ; (x * 2) + (y / 4) >= 3", SMrg (SLit 1) (SBinOp (Comp Ge)
                                                                 (SBinOp (Arith Add)
                                                                      (SBinOp (Arith Mul) (SVar "x") (SLit 2))
                                                                      (SBinOp (Arith Div) (SVar "y") (SLit 4)))
                                                                 (SLit 3)))
               , ("(1 , 2, 3)", STuple [SLit 1,SLit 2,SLit 3])
               , ("(1 ,, 2,, 4)", SMrg (SLit 1) (SMrg (SLit 2) (SLit 4)))
               , ("env.1", SProj SCtx 1)
               , ("env.hello", SRProj SCtx "hello")
               , ("({\"x\" = 10})", SRec "x" (SLit 10))
               , ("({\"x\" = 10} ,, env.x)", (SMrg (SRec "x" (SLit 10)) (SRProj SCtx "x")))]

runTest :: Int -> [(String, SurfaceTm)] -> IO()
runTest n []        = putStrLn $ (show (n + 1) ++ " Tests Completed.")
runTest n (x:xs)    = do
                         if parseSource (fst x) == Just (snd x)
                              then putStrLn $ "Test " ++ (show (n + 1)) ++ ": Passed"
                              else putStrLn $ "Test " ++ (show (n + 1)) ++ ": Failed"
                         runTest (n + 1) xs

test :: IO()
test = runTest 0 test_cases

}