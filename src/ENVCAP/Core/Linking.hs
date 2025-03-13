module ENVCAP.Core.Linking where
import ENVCAP.Syntax 


-- Linking rules
--

-- Link left
-- 
-- Example 1:
-- 
newtype LinkingError = LinkFailed String 
                            deriving Show

example :: CoreTm
example = Mrg (Mrg (Anno Unit TyCUnit) (Anno (Rec "X" (Lit 1)) (TyCRecord "X" TyCInt))) (Rec "Y" (Box Unit (CLam (TyCRecord "X" TyCInt) (Lit 2))))

lookupF   :: CoreTm -> CoreTyp -> Either LinkingError CoreTm
lookupF (Mrg l (Anno tm ty')) ty 
    = if ty' == ty 
        then 
            case lookupF l ty of 
                Right _   -> Left  $ LinkFailed "Ambiguous Fragments"
                Left  _   -> Right tm
        else lookupF l ty
lookupF (Anno tm ty') ty
    = if ty == ty'
        then Right tm
        else Left $ LinkFailed ("Couldn't find the import" ++ show ty')
lookupF _ _ 
    = Left $ LinkFailed "Not well-formed link structure"

link :: CoreTm -> Either LinkingError CoreTm
link (Mrg left (Rec l (Box tm1 (CLam ty tm2))))   
        = link left >>= 
            \left' -> lookupF left' ty >>= \tm -> link (Mrg left' (Rec l (Box (Mrg tm1 tm) tm2)))
            
link (Mrg left (Rec l tm))
        = link left >>= \left' -> return $ Mrg left' (Rec l tm)
link tm = Right tm
