{-# LANGUAGE InstanceSigs #-}
module ENVCAP.Source.Syntax where

data Tm =   TmQuery                               -- Query
        |   TmUnit                                -- Unit
        |   TmString    String                    -- String  Literal
        |   TmInt       Integer                   -- Integer Literal
        |   TmBool      Bool                      -- Boolean Literal
        |   TmAnno      Tm Typ                  -- Type annotation
        |   TmBinary    TmBinaryOp Tm Tm          -- Binary Operation
        {--
        Unary operations: 
                Boolean not ~
                =
        --}
        |   TmUnary     TmUnaryOp Tm              -- Unary Operation
        {--
        Both are valid!

        if {x == True} then {print("Hello")} else {print("False")}
        if (x == True) then {print("Hello")}
        --}
        |   TmIf1       Tm Tm             -- 
        |   TmIf2       Tm Tm Tm          -- Conditional
        {--
        Difference between Var and Lit:
        var can be declared and then, assigned separately
        
        var x : Int; x = 10;            or var x : Int = 10; BOTH are good!
        let x : Int = 10;               This a literal! Assigned value must be an expression and can not be re-assigned (sort of a constant)!

        --}
        |   TmLit       String Typ Tm           -- Literal (It has to be a specific value)
        |   TmVar       String                  -- Variable can change and take different values
        |   TmLet       Tm Tm                   -- Simple Let
        |   TmLetRec    Tm Tm                   -- Let with recursion
        |   TmMrg       Tm Tm                   -- Merge expression
        

        -- |   TmInterface [ModuleDef]               -- Multiple module definitiion
        -- |   TmModule    String SType Tm           -- Module name type expressions
        deriving (Eq, Show)
{--
        Example for Interface:

        Header File

--}

-- type ModuleDef = Defmodule String SType 

{-- Types
data Typ =  TUnit                  -- Unit type for empty environment
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
--}


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
data TmBinaryOp   =     TmArith TmArithOp   -- Arithmetic
                |       TmComp  TmCompOp    -- CompOp
                |       TmLogic TmLogicOp   -- Boolean Logic
        deriving  (Show)

data TmUnaryOp  = TmNeg | TmNot | TmTypeOf |TmIndex Int
                                                deriving (Eq, Show)
data TmArithOp   = TmAdd | TmSub | TmMul | TmDiv | TmMod | TmExp
                                                deriving (Eq, Show)
data TmCompOp    = TmEql | TmNeq | TmLt | TmLe | TmGt | TmGe
                                                deriving (Eq, Show)
data TmLogicOp   = TmAnd | TmOr         deriving (Eq, Show)

-- instance Show TmBinaryOp where
--         show :: TmBinaryOp -> String
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

instance Eq TmBinaryOp where 
        (==) :: TmBinaryOp -> TmBinaryOp -> Bool
        (TmArith op1) == (TmArith op2) = op1 == op2
        (TmComp op1)  == (TmComp op2)  = op1 == op2
        (TmLogic op1) == (TmLogic op2) = op1 == op2
        _           == _           = False

{--
isValue :: Value -> Bool
isValue VUnit                   = True
isValue (VInt _)                = True
isValue (VBool _)               = True
isValue (VClos v t e)           = isValue v
isValue (VRcd label val)        = isValue val
isValue (VMrg v1 v2)            = isValue v1 && isValue v2
--}