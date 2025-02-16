module ENVCAP.Source.PrettyPrint where
import ENVCAP.Syntax
import ENVCAP.Source.Errors (SurfacePrettyPrintError (PrettyPrintFailed))

prettyPrint :: SurfaceTm -> Either SurfacePrettyPrintError String
prettyPrint SCtx    = Right "env()"
prettyPrint _       = Left $ PrettyPrintFailed "Pretty Print Function not completed"