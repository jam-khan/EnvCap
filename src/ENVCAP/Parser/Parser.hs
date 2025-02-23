{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module ENVCAP.Parser.Parser where

import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import ENVCAP.Syntax

type Parser = Parsec Void Text

{-- Interface Parser --}

pProgram :: Parser Interface
pProgram = pInterface

pInterface :: Parser Interface
pInterface = do
    stmt <- pInterfaceStatement
    optional semicolon
    rest <- optional pInterface
    case rest of
        Just r -> return $ InterfaceAnd stmt r
        Nothing -> return stmt

pInterfaceStatement :: Parser Interface
pInterfaceStatement = choice
    [ pTyAliasInterface
    , pFunctionInterface
    , pModuleInterface
    , pBindingInterface
    , pIType
    ]

pTyAliasInterface :: Parser Interface
pTyAliasInterface = do
                    pTokenTyAlias
                    var <- pTokenVar
                    equals
                    typ <- pType
                    return $ IAliasTyp var typ

pFunctionInterface :: Parser Interface
pFunctionInterface = do
    pTokenFunc
    var <- pTokenVar
    params <- parens pParamList
    colon
    typ <- pType
    return $ FunctionTyp var params typ

pModuleInterface :: Parser Interface
pModuleInterface = do
    pTokenModule
    var <- pTokenVar
    params <- parens pParamList
    colon
    typ <- pType
    return $ ModuleTyp var params typ

pBindingInterface :: Parser Interface
pBindingInterface = do
    pTokenValDef
    var <- pTokenVar
    colon
    typ <- pType
    return $ Binding var typ

pIType :: Parser Interface
pIType = IType <$> pType

pType :: Parser SurfaceTyp
pType = choice
    [ STInt <$ pTokenTypeInt
    , STBool <$ pTokenTypeBool
    , STString <$ pTokenTypeString
    , STArrow <$> pType <* arrow <*> pType
    , STAnd <$> pType <* andSymbol <*> pType
    , STList <$> brackets pType
    , STSig <$> (pTokenSig *> brackets (pType <* comma)) <*> pType
    , STIden <$> pTokenVar
    , braces pRecordType
    , parens pType
    ]


pRecordType :: Parser SurfaceTyp
pRecordType = do
    record <- pRecord
    optional comma
    rest <- optional pRecordType
    case rest of
        Just r -> return $ STAnd record r
        Nothing -> return record

pRecord :: Parser SurfaceTyp
pRecord = do
    var <- pTokenVar
    void colon
    STRecord var <$> pType

pParamList :: Parser [(String, SurfaceTyp)]
pParamList = sepBy1 pParam comma

pParam :: Parser (String, SurfaceTyp)
pParam = do
    var <- pTokenVar
    colon
    typ <- pType
    return (var, typ)


{-- Lexer utilities --}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

semicolon :: Parser Text
semicolon = symbol ";"

equals :: Parser Text
equals = symbol "="

arrow :: Parser Text
arrow = symbol "->"

andSymbol :: Parser Text
andSymbol = symbol "&"

plus :: Parser Text
plus = symbol "+"

{-- Tokens Interface --}

pTokenVar :: Parser String
pTokenVar = lexeme (some letterChar)

pTokenSig :: Parser ()
pTokenSig = void $ symbol "Sig"

pTokenTypeInt :: Parser ()
pTokenTypeInt = void $ symbol "Int"

pTokenTypeBool :: Parser ()
pTokenTypeBool = void $ symbol "Bool"

pTokenTypeString :: Parser ()
pTokenTypeString = void $ symbol "String"

pTokenTyAlias :: Parser ()
pTokenTyAlias = void $ symbol "type"

pTokenValDef :: Parser ()
pTokenValDef = void $ symbol "val"

pTokenFunc :: Parser ()
pTokenFunc = void $ symbol "function"

pTokenModule :: Parser ()
pTokenModule = void $ symbol "module"

{-- Run Parser --}

parseInterface :: String -> Either (ParseErrorBundle Text Void) Interface
parseInterface txt = runParser pProgram "" (pack txt)