{-|
Module      : ENVCAP.Manager.Implementation
Description : Module is responsible for parsing and loading of multiple implementation files.
Copyright   : (c) Jam Kabeer Ali Khan, 2025
License     : MIT
Maintainer  : jamkhan@connect.hku.hk
Stability   : experimental

Key functionalities:
- Functionality 1: Parse multiple implementation files.
- Functionality 2: Structure these implementation files.

For more details, see the individual function documentation.
-}
module ENVCAP.Manager.Implementation where

import ENVCAP.Syntax (SourceTm)
import ENVCAP.Source.Errors (SourceTypeError (STypeError))
import System.FilePath (takeBaseName, takeFileName)

type Path     = String
type FileName = String
type Fragment = (FileName, SourceTm)

-- A function to load an implementation file. 
-- 
-- It simply reads the implementation file and returns the AST with its name extracted from path.
-- 
-- === Example:
-- >>> readImplementation "examples/Source/Anonymous.ep"
readImplementation :: Path -> Either SourceTypeError Fragment
readImplementation _filePath = Left $ STypeError "`readImplementation` not implemented yet."


-- A function to extract the file name from the file path.
-- 
-- === Example:
-- >>> getNameFromPath "examples/Source/Anonymous.ep"
-- Right "Anonymous"
--
-- >>> getNameFromPath "examples/Source/Anonymous"
-- Right "Anonymous"
--
-- >>> getNameFromPath "Anonymous"
-- Right "Anonymous"
getNameFromPath :: Path -> FileName
getNameFromPath path    = takeBaseName $ takeFileName path


test :: IO()
test = do
        putStrLn $ "examples/Source/Anonymous.ep  -> " ++ getNameFromPath "examples/Source/Anonymous.ep"
        putStrLn $ "examples/Source/Anonymous     -> " ++ getNameFromPath "examples/Source/Anonymous"
        putStrLn $ "Anonymous                     -> " ++ getNameFromPath "Anonymous"