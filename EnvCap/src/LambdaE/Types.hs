module LambdaE.Types ( Typ(..) ) where

data Typ = 
        TInt                    -- Integer type
    |   TEmpty                  -- Unit type for empty environment
    |   TAnd Typ Typ            -- Intersection type
    |   TArrow Typ Typ          -- Arrow type, e.g. A -> B
    |   TRecord {               -- Single-Field Record Type
            label :: String, 
            typeVal :: Typ }
    deriving (Eq, Show)

