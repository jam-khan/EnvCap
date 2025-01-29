{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module ENVCAP.Source.Syntax where

data Tm =   TmCtx                               -- Query
        |   TmUnit                              -- Unit
        |   TmLit       Integer                 -- Integer Literal
        |   TmBool      Bool                    -- Boolean Literal
        |   TmString    String                  -- String  Literal
        |   TmLam       Typ Tm                  -- Abstraction with binding
        |   TmClos      Tm Tm
        |   TmRec       String Tm
        |   TmRProj     Tm String
        |   TmProj      Tm Int                  -- Projection on Expression
        |   TmApp       Tm Tm
        |   TmMrg       Tm Tm
        |   TmBox       Tm Tm
        -- Extension that require elaboration
        |   TmIf        Tm Tm Tm
        |   TmFix       Tm
        |   TmPair      Tm Tm
        |   TmFst       Tm
        |   TmSnd       Tm
        |   TmNil       Typ
        |   TmCons      Tm Tm
        |   TmBinOp     TmBinOp Tm Tm
        |   TmUnOp      TmUnaryOp Tm
        |   TmCase      Tm
        |   TmInL       Tm
        |   TmInR       Tm
        -- Not sure if tagging is needed at source level -- can be simply added during elaboration to core
        |   TmAnno      Tm Typ                  -- Tm :: Typ
        |   TmTuple     [Tm]
        |   TmSwitch    Tm [(Tm, Tm)]
        |   TmVar       String
        |   TmFunc      String Typ Tm
        |   TmModule    String Typ Typ Tm
        |   TmAliasTyp  String Typ
        |   TmLet       String Typ Tm Tm
        |   TmLetrec    String Typ Tm Tm
        deriving (Eq, Show)

-- Types
data Typ      =     TUnit                  -- Unit type for empty environment
                |   TInt                   -- Integer type
                -- Can be used for pair
                |   TAnd Typ Typ           -- Intersection type
                |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
                |   TRecord String Typ     -- Single-Field Record Type
                -- Extensions
                |   TBool                  -- Boolean type
                |   TString                -- String type
                |   TList  Typ             -- Type for built-in list 
                |   TSum   Typ Typ         -- Type for sums
                |   TPair  Typ Typ
                |   TSig   Typ Typ             -- Sig Type End
                |   TIden  String          -- Simply an alias
                deriving (Eq, Show)

{--

--}

-- Operations Definitions
data TmBinOp   =        TmArith TmArithOp   -- Arithmetic
                |       TmComp  TmCompOp    -- CompOp
                |       TmLogic TmLogicOp   -- Boolean Logic
        deriving  (Show)

data TmUnaryOp  = TmNeg | TmNot | TmTypeOf | TmIndex Int
                                                deriving (Eq, Show)
data TmArithOp   = TmAdd | TmSub | TmMul | TmDiv | TmMod | TmExp
                                                deriving (Eq, Show)
data TmCompOp    = TmEql | TmNeq | TmLt | TmLe | TmGt | TmGe
                                                deriving (Eq, Show)
data TmLogicOp   = TmAnd | TmOr         deriving (Eq, Show)

-- instance Show TmBinOpOp where
--         show :: TmBinOpOp -> String
--         show (TmArith op) = show op
--         show (TmComp op)  = show op
--         show (TmLogic op) = show op
        
-- instance Show TmArithOp where
--         show :: TmArithOp -> String
--         show TmAdd = "+"
--         show TmSub = "-"
--         show TmMul = "*"
--         show TmDiv = "/"
--         show TmMod = "%"
--         show TmExp = "^"

-- instance Show TmCompOp where
--         show :: TmCompOp -> String
--         show TmEql = "=="
--         show TmNeq = "!="
--         show TmLt  = "<"
--         show TmLe  = "<="
--         show TmGt  = ">"
--         show TmGe  = ">="

-- instance Show TmLogicOp where
--         show :: TmLogicOp -> String
--         show TmAnd = "&&"
--         show TmOr  = "||"

-- instance Show TmUnaryOp where
--         show :: TmUnaryOp -> String
--         show TmNot = "!"
--         show TmNeg = "-"
--         show (TmIndex i) = "!!" ++ (show i)
--         show (TmTypeOf)  = "Type of"

instance Eq TmBinOp where 
        (==) :: TmBinOp -> TmBinOp -> Bool
        (TmArith op1) == (TmArith op2) = op1 == op2
        (TmComp op1)  == (TmComp op2)  = op1 == op2
        (TmLogic op1) == (TmLogic op2) = op1 == op2
        _           == _           = False