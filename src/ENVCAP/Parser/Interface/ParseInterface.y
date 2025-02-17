{
{-# OPTIONS_GHC -Werror=non-exhaustive-patterns #-}
module ENVCAP.Parser.Interface.ParseInterface where
import Data.Char
import ENVCAP.Syntax 
}

%name interfaceParser
%tokentype          { Token        }
%error              { parseError   }

%token
     var            {    TokenVar $$         }
     'Sig'          {    TokenSig            }          
     'Int'          {    TokenTypeInt        }
     'Bool'         {    TokenTypeBool       }
     'String'       {    TokenTypeString     }
     '->'           {    TokenTypeArrow      }
     '&'            {    TokenTypeAnd        }
     'type'         {    TokenTyAlias        }
     'val'          {    TokenValDef         }
     'function'     {    TokenFunc           }
     'module'       {    TokenModule         }
     '['            {    TokenOpenSqBracket  }
     ']'            {    TokenCloseSqBracket }
     ','            {    TokenComma          }
     '+'            {    TokenPlus           }
     '('            {    TokenOB             }
     ')'            {    TokenCB             }
     ';'            {    TokenSemicolon      }
     ':'            {    TokenColon          }
     '='            {    TokenEq             }
     '{'            {    TokenOpenBracket    }
     '}'            {    TokenCloseBracket   }

%right '='
%left '->'
%left '&'
%left '+'

%%

Program             : Interface  { $1 }

Interface           : InterfaceStatement ';' Interface                {    InterfaceAnd $1 $3   }
                    | InterfaceStatement                              { $1 }

InterfaceStatement  : TyAliasInterface                                { $1 }
                    | FunctionInterface                               { $1 }
                    | ModuleInterface                                 { $1 }
                    | BindingInterface                                { $1 }
                    | IType                                           { $1 }

TyAliasInterface    : 'type' var '=' Type                             {    IAliasTyp $2 $4   }

FunctionInterface   : 'function' var '(' ParamList ')' ':' Type       {    FunctionTyp $2 $4 $7     }

ModuleInterface     : 'module'   var '(' ParamList ')' ':' Type       {    ModuleTyp   $2 $4 $7     }

BindingInterface    : 'val'      var ':' Type                         {    Binding $2 $4            }

IType               : Type                                            {    IType $1  }

Type                : 'Int'                                           {    STInt     }
                    | 'Bool'                                          {    STBool    }
                    | 'String'                                        {    STString  }
                    | Type '->' Type                                  {    STArrow $1 $3   }
                    | Type '&'  Type                                  {    STAnd   $1 $3   }
                    | '[' Type ']'                                    {    STList $2       }
                    | 'Sig' '[' Type ',' Type ']'                     {    STSig $3 $5     }
                    | var                                             {    STIden $1       }
                    | '{' RecordType '}'                              {    $2   }
                    | '(' Type ')'                                    {    $2   }


RecordType          : Record ',' RecordType                           {    STAnd $1 $3     }
                    | Record                                          {    $1   }

Record              : var ':' Type                                    {    STRecord $1 $3  }   

ParamList           : Param ',' ParamList                             {  $1 : $3  }
                    | Param                                           {   [$1]   }

Param               : var ':' Type                          { ($1, $3) } 

{

parseError :: [Token] -> a
parseError (x:xs)   = error ("Parse Error: Token Failed" ++ show x)
parseError []       = error ("Parser Error: No Tokens")

data Token =   TokenVar String        --  x
          |    TokenPlus              -- '+'
          |    TokenOB                -- '('
          |    TokenCB                -- ')'
          |    TokenEq                -- '='
          |    TokenSemicolon         -- ';'
          |    TokenValDef            -- 'val'
          |    TokenFunc              -- 'function'
          |    TokenModule            -- 'module'
          |    TokenOpenBracket       -- '{'
          |    TokenCloseBracket      -- '}'
          |    TokenColon             -- ':'
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
          deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexVar (c:cs)
lexer ('+':cs)      = TokenPlus                        : lexer cs
lexer ('-':cs)      = case cs of
                         ('>':cs') -> TokenTypeArrow   : lexer cs'
lexer ('=':cs)      = TokenEq                          : lexer cs
lexer ('&':cs)      = TokenTypeAnd                     : lexer cs
lexer (';':cs)      = TokenSemicolon                   : lexer cs
lexer (',':cs)      = TokenComma                       : lexer cs
lexer (':':cs)      = TokenColon                       : lexer cs 
lexer ('{':cs)      = TokenOpenBracket                 : lexer cs
lexer ('}':cs)      = TokenCloseBracket                : lexer cs
lexer ('[':cs)      = TokenOpenSqBracket               : lexer cs
lexer (']':cs)      = TokenCloseSqBracket              : lexer cs
lexer ('(':cs)      = TokenOB                          : lexer cs
lexer (')':cs)      = TokenCB                          : lexer cs

lexVar cs           = case span isAlpha cs of
                         ("Int",        rest)     -> TokenTypeInt     : lexer rest
                         ("Bool",       rest)     -> TokenTypeBool    : lexer rest
                         ("Sig",        rest)     -> TokenSig         : lexer rest
                         ("String",     rest)     -> TokenTypeString  : lexer rest
                         ("type",       rest)     -> TokenTyAlias     : lexer rest
                         ("function",   rest)     -> TokenFunc        : lexer rest
                         ("module",     rest)     -> TokenModule      : lexer rest
                         ("val",        rest)     -> TokenValDef      : lexer rest
                         (var,          rest)     -> TokenVar var     : lexer rest

parseInterface :: String -> Maybe Interface
parseInterface input 
     = case interfaceParser (lexer input) of
          result -> Just result
          _      -> Nothing                    

}