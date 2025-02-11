module ENVCAP.Interpreter where
import ENVCAP.Syntax (SurfaceTm, SurfaceTyp (STUnit))
import ENVCAP.Parser.Happy (parseSource)



-- First pass
{-- 
    Parser ~~~~~> 
    Surface level ~~~(Type Expansion)~~> 
    Surface level ~~~(Locally nameless and scope handling)~~~>
    Surface level ~~~(Desugaring)~~~> 
        Source(Core ENVCAP) ~~~~(Type-Directed Elaboration)~~~~>
            Core LambdaE ~~~(Core Type Checker (Redundant if above is done correct))~~~>
                Big-step (evaluator)
--}

-- run :: String -> Maybe SurfaceTm
-- run code = parseSource code >>= \parsedTm -> expandAlias STUnit parsedTm
