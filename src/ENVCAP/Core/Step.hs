module ENVCAP.Core.Step where

import ENVCAP.Syntax 

-- This is the implementation of small-step operational semantics
-- utility is for the property-based testing of equivalence between big-step and small-step
-- 
step :: CoreTm -> Maybe CoreTm
step _ = Nothing