module ENVCAP.Source.Errors where

-- | Represents errors that can occur during type expansion.
data TypeExpansionError
    =   AliasNotFound       String  -- ^ Type alias not found in the context.
    |   DuplicateAlias      String  -- ^ Multiple types with same alias detected (Not allowed).
    |   TypeContextError    String  -- ^ If typing context is not well formed with intersection types
    |   TypeExpansionFailed String  -- ^ Type expansion failed at the term level.
    deriving Show

-- | Represents errors that can occur during the pretty printing of the surface level AST.
newtype SurfacePrettyPrintError = PrettyPrintFailed String -- ^ Simple print failure

-- | IMPORTANT: Description not completed
data LocallyNamelessError
    = ScopeError            String  -- ^ Potential misuse
    | ParamError            String
    | DebruijnFailed        String  -- ^ Failed while resolving de-bruijn indices
    | LocallyNamelessFailed String
    deriving Show

-- | IMPORTANT: Description not completed
newtype DesugarError = DesugarFailed String
    deriving Show

-- | Interpreter error 
newtype InterpreterError = InterpreterFailed String
    deriving Show

-- | Represents type errors detected at the source level.
newtype SourceTypeError = STypeError String 
    deriving Show

-- | Represents errors for separate compilation.
newtype SeparateCompilationError = SepCompError String
    deriving Show 