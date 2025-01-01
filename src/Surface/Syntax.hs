{-# LANGUAGE InstanceSigs #-}
module Surface.Syntax where

{--
program
        : expression+
        ;

expresssion
        : context
        | unit
        | 

--}
-- EnvCap features
-- 
data Tm =   TmCtx                              -- Context
        |   TmUnit                             -- Unit
        |   TmVar       String              -- Variable
        |   TmString    String                 -- String  Literal
        |   TmInt       Integer                -- Integer Literal
        |   TmBool      Bool                   -- Boolean Literal
        |   TmBinary    TmBinaryOp Tm Tm       -- Binary Operation
        |   TmUnary     TmUnaryOp Tm           -- Unary Operation
        |   TmIf        Tm Tm Tm               -- Conditional
        -- |   Tm
-- data Term =  TCtx                     -- Context
--         |   Unit                    -- Unit
--         |   Lit    Int               -- Integer literal
--         |   BinOp  BinaryOp Exp Exp  -- Binary operations: Application, Box and Merge
--         |   Lam    Typ Exp           -- Lambda Abstraction
--         |   Proj   Exp Int           -- Projection
--         |   Clos   Exp Typ Exp       -- Closure
--         |   Rec    String Exp        -- Single-Field Record
--         |   RProj  Exp String        -- Record Projection by Label
--         -- Extensions
--         |   UnOp   UnaryOp Exp       -- Unary operations:  Not
--         |   EBool  Bool              -- Boolean Term
--         |   If     Exp Exp Exp       -- Conditionals
--         |   Let    Exp Exp           -- Let Bindings
--         |   Fix    Exp               -- Recursion
--         -- Pairs
--         |   Pair   Exp Exp           -- Pair
--         |   Fst    Exp               -- Left projection
--         |   Snd    Exp               -- Right projection
--         -- Sums
--         |   Inl    Typ Exp                 -- tagging left
--         |   Inr    Typ Exp                 -- tagging right
--         -- Built-in Lists
--         |   Nil    Typ                -- Nil for list
--         |   Cons   Exp Exp            -- List
--         |   Head   Exp                -- Head of List
--         |   Tail   Exp                -- Tail of List
--         -- Case match for Sums and Lists
--         |   Case   Exp Exp
        deriving (Eq, Show)

-- Operations Definitions
data TmBinaryOp   =     TmApp             -- Application
                |       TmBox             -- Box
                |       TmMrg             -- Merge
                --      Extensions
                |       TmArith TmArithOp   -- Arithmetic
                |       TmComp  TmCompOp    -- CompOp
                |       TmLogic TmLogicOp   -- Boolean Logic


{--
-- Values
data Value =    VUnit                   -- Unit value
        |       VInt Int                -- Integer value
        |       VClos Value Typ Tm      -- Closure
        |       VRcd String Value       -- Single-field record value
        |       VMrg Value Value        -- Merge of two values
        -- Extensions
        |       VBool Bool              -- Boolean Value
        |       VNil Typ                -- Nil for list
        |       VCons Value Value       -- List
        --      
        deriving Eq
--}

-- Types
data Typ =  TUnit                  -- Unit type for empty environment
        |   TInt                   -- Integer type
        |   TAnd Typ Typ           -- Intersection type
        |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
        |   TRecord String Typ     -- Single-Field Record Type
        -- Extensions
        |   TBool                  -- Boolean type
        |   TList  Typ             -- Type for built-in list
        |   TSum   Typ Typ
        deriving Eq


data TmUnaryOp  = TmNeg | TmNot | TmIndex Int
                                                deriving Eq
data TmArithOp   = TmAdd | TmSub | TmMul | TmDiv | TmMod | TmExp
                                                deriving Eq
data TmCompOp    = TmEql | TmNeq | TmLt | TmLe | TmGt | TmGe
                                                deriving Eq
data TmLogicOp   = TmAnd | TmOr         deriving Eq

instance Show TmBinaryOp where
        show :: TmBinaryOp -> String
        show (TmArith op) = show op
        show (TmComp op)  = show op
        show (TmLogic op) = show op
        show TmApp        = "App"
        show TmBox        = "\x25B8"
        show TmMrg        = " ,, "

instance Show TmArithOp where
        show :: TmArithOp -> String
        show TmAdd = "+"
        show TmSub = "-"
        show TmMul = "*"
        show TmDiv = "/"
        show TmMod = "%"

instance Show TmCompOp where
        show :: TmCompOp -> String
        show TmEql = "=="
        show TmNeq = "!="
        show TmLt  = "<"
        show TmLe  = "<="
        show TmGt  = ">"
        show TmGe  = ">="

instance Show TmLogicOp where
        show :: TmLogicOp -> String
        show TmAnd = "&&"
        show TmOr  = "||"

instance Show TmUnaryOp where
        show :: TmUnaryOp -> String
        show TmNot = "!"
        show TmNeg = "-"
        show (TmIndex i) = "!!" ++ (show i)

instance Eq TmBinaryOp where 
        (==) :: TmBinaryOp -> TmBinaryOp -> Bool
        (TmArith op1) == (TmArith op2) = op1 == op2
        (TmComp op1)  == (TmComp op2)  = op1 == op2
        (TmLogic op1) == (TmLogic op2) = op1 == op2
        TmApp         == TmApp         = True
        TmBox         == TmBox         = True
        TmMrg         == TmMrg         = True
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