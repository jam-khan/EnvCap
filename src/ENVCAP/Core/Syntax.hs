{-# LANGUAGE InstanceSigs #-}
module ENVCAP.Core.Syntax where

import Test.QuickCheck (Property, property, quickCheck, Arbitrary(arbitrary), Gen, elements, oneof )


data Exp =  Ctx                      -- Context
        |   Unit                     -- Unit
        |   Lit    Integer           -- Integer literal
        |   Lam    Typ Exp           -- Lambda Abstraction
        |   Proj   Exp Int           -- Projection
        |   Clos   Exp Exp           -- Closure
        |   Rec    String Exp        -- Single-Field Record
        |   RProj  Exp String        -- Record Projection by Label
        |   App    Exp Exp           -- Application
        |   Mrg    Exp Exp           -- Merge
        |   Box    Exp Exp           -- Box
        -- Extensions
        |   EBool   Bool             -- Boolean Term
        |   EString String           -- String Term
        |   If     Exp Exp Exp       -- Conditionals
        |   Let    Exp Exp           -- Let Bindings
        |   Fix    Exp               -- Recursion
        -- Pairs
        |   Pair   Exp Exp           -- Pair
        |   Fst    Exp               -- First Projection
        |   Snd    Exp               -- Second Projection
        -- Sums
        |   InL    Typ Exp           -- Tagging Left
        |   InR    Typ Exp           -- Tagging Right
        |   Case   Exp Exp Exp       -- Case of Sums
        -- Built-in Lists
        |   Nil    
        |   Cons   Exp Exp           -- Cons for List
        |   LCase  Exp Exp Exp       -- Case of List
        -- Operations
        |   BinOp  BinaryOp Exp Exp  -- Binary operations
        |   UnOp   UnaryOp Exp       -- Unary operations
        deriving (Eq, Show)

{--
        Maintaining a different AST for Value for Big Step operational semantics.
        Potentially, utilize small step for step-wise debugging later!
--}

-- Values
data Value =    VUnit                      -- Unit value
        |       VInt    Integer            -- Integer value
        |       VClos   Value Exp          -- Closure
        |       VRcd    String Value       -- Single-field record value
        |       VMrg    Value Value        -- Merge of two values
        -- Extensions
        |       VBool   Bool               -- Boolean Value
        |       VString String             -- String Value
        -- Pair extension
        |       VPair   Value Value        -- Pair value
        -- Sums extension
        |       VInL    Typ Value          -- tagged value (left)
        |       VInR    Typ Value          -- tagged value (right)
        -- Lists extension
        |       VNil    
        |       VCons   Value Value        -- List
        deriving (Eq, Show)

-- Types
data Typ =  TUnit                       -- Unit type for empty environment
        |   TInt                        -- Integer type
        -- Can be used for pair
        |   TAnd        Typ Typ         -- Intersection type
        |   TArrow      Typ Typ         -- Arrow type, e.g. A -> B
        |   TRecord     String Typ      -- Single-Field Record Type
        -- Extensions
        |   TBool                       -- Boolean type
        |   TString                     -- String type
        |   TList       Typ             -- Type for built-in list
        |   TSum        Typ Typ         -- Type for sums
        |   TPair       Typ Typ
        deriving (Eq, Show)

data UnaryOp    =       Not
        deriving (Eq, Show)

-- Operations Definitions
data BinaryOp  =        Arith ArithOp   -- Arithmetic
                |       Comp  CompOp    -- CompOp
                |       Logic LogicOp   -- Boolean Logic
                deriving (Eq, Show)

data ArithOp = Add | Sub | Mul | Div | Mod
        deriving (Eq, Show)

data CompOp  = Eql | Neq | Lt | Le | Gt | Ge
        deriving (Eq, Show)

data LogicOp = And | Or
        deriving (Eq, Show)

instance Arbitrary Typ where
    arbitrary :: Gen Typ
    arbitrary = oneof [ return TUnit,
                        return TInt,
                        return TBool,
                        return TString,
                        TAnd    <$> arbitrary <*> arbitrary,
                        TArrow  <$> arbitrary <*> arbitrary,
                        TRecord <$> arbitrary <*> arbitrary,
                        TList   <$> arbitrary,
                        TSum    <$> arbitrary <*> arbitrary,
                        TPair   <$> arbitrary <*> arbitrary]

instance Arbitrary Exp where
    arbitrary :: Gen Exp
    arbitrary = oneof [ return Ctx
                      , return Unit
                      , return Nil
                      , Lit     <$> arbitrary
                      , Lam     <$> arbitrary <*> arbitrary
                      , Proj    <$> arbitrary <*> arbitrary
                      , Clos    <$> arbitrary <*> arbitrary
                      , Rec     <$> arbitrary <*> arbitrary
                      , RProj   <$> arbitrary <*> arbitrary
                      , App     <$> arbitrary <*> arbitrary
                      , Mrg     <$> arbitrary <*> arbitrary
                      , Box     <$> arbitrary <*> arbitrary
                      , EBool   <$> arbitrary
                      , EString <$> arbitrary
                      , If      <$> arbitrary <*> arbitrary <*> arbitrary
                      , Let     <$> arbitrary <*> arbitrary
                      , Fix     <$> arbitrary
                      , Pair    <$> arbitrary <*> arbitrary
                      , Fst     <$> arbitrary
                      , Snd     <$> arbitrary
                      , InL     <$> arbitrary <*> arbitrary
                      , InR     <$> arbitrary <*> arbitrary
                      , Case    <$> arbitrary <*> arbitrary <*> arbitrary
                      , Cons    <$> arbitrary <*> arbitrary
                      , LCase   <$> arbitrary <*> arbitrary <*> arbitrary
                      , BinOp   <$> arbitrary <*> arbitrary <*> arbitrary
                      , UnOp    <$> arbitrary <*> arbitrary]

instance Arbitrary Value where
        arbitrary :: Gen Value
        arbitrary = oneof [
                        return VUnit,
                        return VNil,
                        VInt    <$> arbitrary,
                        VBool   <$> arbitrary,
                        VString <$> arbitrary,
                        VClos   <$> arbitrary <*> arbitrary,
                        VRcd    <$> arbitrary <*> arbitrary,
                        VMrg    <$> arbitrary <*> arbitrary,
                        VPair   <$> arbitrary <*> arbitrary,
                        VInL    <$> arbitrary <*> arbitrary,
                        VInR    <$> arbitrary <*> arbitrary,
                        VCons   <$> arbitrary <*> arbitrary]

instance Arbitrary BinaryOp where
        arbitrary :: Gen BinaryOp
        arbitrary = oneof [
                        Arith <$> arbitrary,
                        Comp  <$> arbitrary,
                        Logic <$> arbitrary]

instance Arbitrary ArithOp where
        arbitrary :: Gen ArithOp
        arbitrary = oneof [
                        return Add,
                        return Sub,
                        return Mul,
                        return Div,
                        return Mod]

instance Arbitrary CompOp where
        arbitrary :: Gen CompOp
        arbitrary = oneof [
                        return Eql,
                        return Neq,
                        return Lt,
                        return Le,
                        return Gt,
                        return Ge]

instance Arbitrary LogicOp where
        arbitrary :: Gen LogicOp
        arbitrary = oneof [
                        return And,
                        return Or]

instance Arbitrary UnaryOp where
        arbitrary :: Gen UnaryOp
        arbitrary = return Not

-- instance Show BinaryOp where
--         show :: BinaryOp -> String
--         show (Arith op) = show op
--         show (Comp op)  = show op
--         show (Logic op) = show op

-- instance Show ArithOp where
--         show :: ArithOp -> String
--         show Add = "+"
--         show Sub = "-"
--         show Mul = "*"
--         show Div = "/"
--         show Mod = "%"

-- instance Show CompOp where
--         show :: CompOp -> String
--         show Eql = "=="
--         show Neq = "!="
--         show Lt  = "<"
--         show Le  = "<="
--         show Gt  = ">"
--         show Ge  = ">="

-- instance Show LogicOp where
--         show :: LogicOp -> String
--         show And = "&&"
--         show Or  = "||"

-- instance Show UnaryOp where
--         show :: UnaryOp -> String
--         show Not = "!"

-- instance Show Exp where
--         show :: Exp -> String
--         show                = showIndented 0

-- showIndented :: Int -> Exp -> String
-- showIndented n exp = indent 0 ++ show' n exp
--         where
--         indent :: Int -> String
--         indent n1 = replicate n1 ' '

--         show' :: Int -> Exp -> String
--         show' _ Ctx                = "?"
--         show' _ Unit               = "\x03B5"
--         show' _ (Lit i)            = show i
--         show' n (BinOp op e1 e2)   = "(" ++ show' n e1 ++ " " ++ show op ++ " " ++ show' n e2 ++ ")"
--         show' n (UnOp op e)        = show op ++ "(" ++ show' n e ++ ")"
--         show' n (Lam typ e)        =  "\n"
--                                         ++ indent n ++ "\x03BB" ++ " " ++ show typ ++ " . \n"
--                                                 ++ indent (n + 2) ++ show' (n + 2) e
--         show' n (Proj e n')        = show' n e ++ "." ++ show n'
--         show' n (Clos e1 e2)       = "< " ++ show' n e1 ++ ", " ++ show' (n + 2) e2 ++ " >"
--         show' n (Rec s e)          = "{ " ++ show s  ++ " = " ++ show' (n + 2) e ++ " }"
--         show' n (RProj e s)        = show' n e ++ "." ++ show s
--         show' n (If c e1 e2)       = "IF " ++ show' (n + 2) c ++ "\n"
--                                         ++ indent (n + 4) ++ "THEN \n" ++ indent (n + 6) ++ show' (n + 4) e1 ++ "\n"
--                                         ++ indent (n + 4) ++ "ELSE \n" ++ indent (n + 6) ++ show' (n + 4) e2 ++ "\n"
--         show' n (Let e1 e2)        =    "LET \n"
--                                                 ++ indent n ++ show' n e1 ++
--                                         "\n IN "
--                                                 ++ indent n ++ show' (n + 2) e2
--         show' _ (EBool b)          = show b
--         show' n (Fix e)            = indent n ++ "fix " ++ show' (n + 2) e
--         show' n (Pair e1 e2)       = "(" ++ show' n e1 ++ ", " ++ show' n e2 ++ ")"
--         show' _ (Nil typ)          = indent n ++ "NIL of " ++ show typ
--         show' n (Cons e1 e2)       = indent n ++ show' n e1 ++ " :: " ++ show' n e2
--         show' n (App e1 e2)        = "(" ++ show' n e1 ++ ")" ++ "(" ++ show' n e2 ++ ")"
--         show' n (Box e1 e2)        = "(" ++ show' n e1 ++ "\x25B8" ++ show' n e2 ++ ")"
--         show' n (Mrg e1 e2)        = "(" ++ show' n e2 ++ ")" ++ " ,, " ++ "(" ++ show' n e2 ++ ")"
--         show' n _                  = ""

-- instance Show Typ where
--         show :: Typ -> String
--         show TUnit              = "\x03B5"
--         show TInt               = "INT"
--         show TBool              = "BOOL"
        
--         show (TAnd t1 t2)       = show t1 ++ " & " ++ show t2
--         show (TArrow t1 t2)     = show t1 ++ " -> " ++ show t2
--         show (TRecord s t)      = "{ " ++ show s ++ " :: " ++ show t ++ " }"
--         show (TList typ)        = "[" ++ show typ ++ "]"
-- Values
{--
        data Value =    VUnit                      -- Unit value
                |       VInt    Integer            -- Integer value
                |       VClos   Value Exp          -- Closure
                |       VRcd    String Value       -- Single-field record value
                |       VMrg    Value Value        -- Merge of two values
                -- Extensions
                |       VBool   Bool               -- Boolean Value
                |       VString String             -- String Value
                -- Pair extension
                |       VPair   Value Value        -- Pair value
                -- Sums extension
                |       VInL    Typ Value          -- tagged value (left)
                |       VInR    Typ Value          -- tagged value (right)
                -- Lists extension
                |       VNil    Typ                -- Nil for list
                |       VCons   Value Value        -- List
                deriving Eq
--}
-- instance Show Value where
--         show :: Value -> String
--         show VUnit              = "\x03B5"
--         show (VInt i)           = show i
--         show (VBool b)          = show b
--         show (VString s)        = "\'" ++ show s ++ "\'"
--         show (VClos val exp)    = "closure<" ++ show val ++ show exp ++ ">"
--         show (VRcd label val)   = "{" ++ show label ++ ": " ++ show val ++ "}"
--         show (VMrg v1 v2)       = show v1 ++ " ,, " ++ show v2
--         show (VNil typ)         = "List<[nil " ++ show typ ++ "]>"
--         show (VCons head rest)  = "List<[" ++ show head ++ ", " ++ show rest ++ "]>"
--         show (VPair v1 v2)      = "Pair[(" ++ show v1 ++ ", " ++ show v2 ++ ")]"
--         show (VInL typ value)   = "Left(" ++ show value ++ ")"

-- instance Show Value where
--         show :: Value -> String
--         show VUnit               = "\x03B5"
--         show (VInt i)            = show i
--         show (VBool b)           = show b
--         show (VClos val exp)     =   "< { " ++ show val ++ " }, \n(" ++ showIndented 6 exp ++ "}"
--         show (VRcd label val)    = "{ " ++ show label ++ " = "  ++ show val ++ " }"
--         show (VMrg v1 v2)        = show v1 ++ " ,, " ++ show v2
--         show (VNil typ)          = "nil of " ++ show typ
--         show (VCons head rest)   = show head ++ " :: " ++ show rest