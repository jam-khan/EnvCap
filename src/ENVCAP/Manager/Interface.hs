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

parseHeader :: String