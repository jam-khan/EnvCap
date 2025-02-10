module ENVCAP.Core.Step where

import ENVCAP.Syntax ( Exp )

-- This is the implementation of small-step operational semantics
-- utility is for the property-based testing of equivalence between big-step and small-step
-- 
step :: Exp -> Maybe Exp
step _ = Nothing