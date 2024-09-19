module LambdaE.Check where

import LambdaE.Syntax (Op(..), Expr(..))
import LambdaE.Types ( Typ (..) )

-- ******* Potential Improvement? ********
data LookupResult = Found Typ | NotFound
    deriving (Eq, Show)

-- Look up function for Types
lookupType :: Typ -> Int -> LookupResult
lookupType (TAnd left right) n
    | n == 0 = Found left
    | n == 1 = Found right
    | otherwise = case lookupType left (n - 1) of
                    Found foundType -> Found foundType
                    NotFound        -> lookupType right (n - 1)
lookupType _ _ = NotFound

-- ******* Testing ********
-- ******* Potential Improvement? ********
