module LambdaE.Types ( Typ(..) ) where

data Typ = 
        TInt                    -- Integer type
    |   TEmpty                  -- Unit type for empty environment
    |   TAnd Typ Typ            -- Intersection type
    |   TArrow Typ Typ          -- Arrow type, e.g. A -> B
    |   TRecord { label :: String, typeVal :: Typ } -- Single-Field Record Type
    deriving (Eq, Show)
