module LambdaE.Check where

import LambdaE.Syntax (Op(..), Expr(..))
import LambdaE.Types ( Typ (..) )

-- ******* Potential Improvement? ********
data LookupResultT = Found Typ | NotFound
    deriving (Eq, Show)

-- Look up function for Types
lookupType :: Typ -> Int -> LookupResultT
lookupType (TAnd left right) 0 = Found left
lookupType (TAnd left right) 1 = Found right
lookupType (TAnd left right) n =
    let res = lookupType left (n - 1)
        in case res of 
            NotFound -> lookupType right (n - 1)
            _        -> res
lookupType _ _ = NotFound

-- ******* Testing ********
-- ******* Potential Improvement? ********
