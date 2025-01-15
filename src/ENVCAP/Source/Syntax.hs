{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module ENVCAP.Source.Syntax where

data Tm =   TmCtx                               -- Query
        |   TmUnit                              -- Unit
        |   TmLit       Integer                 -- Integer Literal
        |   TmBool      Bool                    -- Boolean Literal
        |   TmString    String                  -- String  Literal
        |   TmLam       [(String, Typ)] Tm      -- Abstraction with binding
        |   TmProj      Tm Int                  -- Projection on Expression
        |   TmClos      Tm Tm
        |   TmRec       String Tm
        |   TmRProj     String Tm
        |   TmApp       Tm Tm
        |   TmMrg       Tm Tm
        |   TmBox       Tm Tm
        |   TmIf        Tm Tm Tm
        |   TmLet       String Typ Tm Tm
        |   TmLetrec    String Typ Tm Tm
        |   TmTuple     [Tm]
        |   TmFst       Tm
        |   TmSnd       Tm
        |   TmNil       Typ
        |   TmCons      Tm Tm
        |   TmBinOp     TmBinOpOp Tm Tm
        |   TmUnOp      TmUnaryOp Tm
        |   TmCase      Tm                      -- This will perform type-directed elaboration to different case in core
        -- Extension that require elaboration
        -- Type annotation
        |   TmInL       Tm
        |   TmInR       Tm
        -- Not sure if tagging is needed at source level -- can be simply added during elaboration to core
        |   TmAnno      Tm Typ                  -- Tm : Typ
        |   TmSwitch    Tm [(Tm, Tm)]           -- Match/Switch
        |   TmSeq       Tm Tm                   -- Sequence (Not sure abt this)
        |   TmVar       String
        deriving (Eq, Show)

data TypeVar = TVar String Typ

data Module     = Module Import Export [Tm] deriving (Eq, Show)
type Import     = [Typ]
type Export     = [Typ]

data Header     = Header Import Export deriving (Eq, Show)

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
                deriving (Eq, Show)

-- Operations Definitions
data TmBinOpOp   =     TmArith TmArithOp   -- Arithmetic
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

instance Eq TmBinOpOp where 
        (==) :: TmBinOpOp -> TmBinOpOp -> Bool
        (TmArith op1) == (TmArith op2) = op1 == op2
        (TmComp op1)  == (TmComp op2)  = op1 == op2
        (TmLogic op1) == (TmLogic op2) = op1 == op2
        _           == _           = False