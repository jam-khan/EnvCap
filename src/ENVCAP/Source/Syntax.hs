{-# LANGUAGE InstanceSigs #-}
module ENVCAP.Source.Syntax where

{--
data Exp =  Ctx                      -- Context
        |   Unit                     -- Unit
        |   Lit    Integer           -- Integer literal
        |   EBool   Bool             -- Boolean Term
        |   EString String           -- String Term
        |   Lam    Typ Exp           -- Lambda Abstraction
        |   Proj   Exp Int           -- Projection
        |   Clos   Exp Exp           -- Closure
        |   Rec    String Exp        -- Single-Field Record
        |   RProj  Exp String        -- Record Projection by Label
        |   App    Exp Exp           -- Application
        |   Mrg    Exp Exp           -- Merge
        |   Box    Exp Exp           -- Box
        -- Extensions
        |   If     Exp Exp Exp       -- Conditionals
        |   Let    Exp Exp           -- Let Bindings
        |   Fix    Exp               -- Recursion
        -- Above extensions look good
        -- Pairs
        |   Pair   Exp Exp           -- Pair
        |   Fst    Exp               -- First Projection
        |   Snd    Exp               -- Second Projection
        -- Sums
        |   InL    Typ Exp           -- Tagging Left
        |   InR    Typ Exp           -- Tagging Right
        |   Case   Exp Exp Exp       -- Case of Sums
        -- Built-in Lists
        |   Nil    Typ               -- Nil List
        |   Cons   Exp Exp           -- Cons for List
        |   LCase  Exp Exp Exp       -- Case of List
        -- Operations
        |   BinOp  BinaryOp Exp Exp  -- Binary operations
        |   UnOp   UnaryOp Exp       -- Unary operations
        deriving (Eq, Show)
--}
data Tm =   TmCtx                               -- Query
        |   TmUnit                              -- Unit
        |   TmLit       Integer                 -- Integer Literal
        |   TmBool      Bool                    -- Boolean Literal
        |   TmString    String                  -- String  Literal
        |   TmLam       String Typ Tm           -- Abstraction with binding
        |   TmProj      Tm Int                  -- Projection on Expression
        |   TmClos      Tm Tm
        |   
        |   TmBinary    TmBinaryOp Tm Tm        -- Binary Operation
        |   TmUnary     TmUnaryOp Tm            -- Unary Operation
        |   TmIf       Tm Tm Tm                 -- Conditional
        |   TmVar       String                  -- Variable can change and take different values
        |   TmLet       Tm Tm                   -- Simple Let
        |   TmLetRec    Tm Tm                   -- Let with recursion
        |   TmMrg       Tm Tm                   -- Merge expression
        |   Tm
        |   TmAnno      Tm Typ                  -- Type annotation
        |   Interface                           -- Multiple module definitiion
        |   Module    String Typ Tm             -- Module name type expressions
        deriving (Eq, Show)
{--
        Example for Interface:

        Header File

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