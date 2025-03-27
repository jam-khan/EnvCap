{-|
Module      : ENVCAP.Manager.Interface
Description : Module is responsible for parsing and loading of multiple interface files.
Copyright   : (c) Jam Kabeer Ali Khan, 2025
License     : MIT
Maintainer  : jamkhan@connect.hku.hk
Stability   : experimental

Key functionalities:
- Functionality 1: Parse interface files.
- Functionality 2: Structure interface files.

For more details, see the individual function documentation.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ENVCAP.Manager.Interface where
import ENVCAP.Source.Errors
import ENVCAP.Parser.Interface.ParseInterface (parseInterface)
import ENVCAP.Syntax
import ENVCAP.Source.TypeExpansion (expandTyAlias, expandAliasTypParams)
import ENVCAP.Source.Desugar (getFixpointType, desugarTyp)
import Control.Exception
import System.FilePath ((</>))
import System.IO (readFile)

parseHeader :: FilePath -> IO (Either SeparateCompilationError ParseIntfData)
parseHeader filePath = 
    do  fileContent <- try (readFile filePath) :: IO (Either IOException String)
        case fileContent of
            Left  e       -> return $ Left $ SepCompError $ show e
            Right content -> 
                case parseInterface content of
                    Nothing     -> return $ Left $ SepCompError ("Interface Parsing Failed for file: " ++ filePath)
                    Just result -> return $ Right result