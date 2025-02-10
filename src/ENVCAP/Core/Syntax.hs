{-# LANGUAGE InstanceSigs #-}
module ENVCAP.Core.Syntax where

import Test.QuickCheck ( Arbitrary(arbitrary), oneof, Gen)

data Exp =  Ctx                      -- Context
        |   Unit                     -- Unit
        |   Lit    Integer           -- Integer literal
        |   Lam    Typ Exp           -- Lambda Abstraction
        |   Proj   Exp Int           -- Projection
        |   Clos   Exp  Exp          -- Closure
        |   Rec    String Exp        -- Single-Field Record
        |   RProj  Exp String        -- Record Projection by Label
        |   App    Exp Exp           -- Application
        |   Mrg    Exp Exp           -- Merge
        |   Box    Exp Exp           -- Box
        -- Extensions
        |   EBool   Bool             -- Boolean Term
        |   EString String           -- String Term
        |   If     Exp Exp Exp       -- Conditionals
        |   Fix    Typ Exp           -- Recursion
        |   Pair   Exp Exp           -- Pair
        |   Fst    Exp               -- First Projection
        |   Snd    Exp               -- Second Projection
        |   InL    Typ Exp           -- Tagging Left
        |   InR    Typ Exp           -- Tagging Right
        |   Case   Exp Exp Exp       -- Case of Sums
        |   Nil    Typ               -- Nil typ, e.g. [] of Int
        |   Cons   Exp Exp           -- Cons for List
        |   LCase  Exp Exp Exp       -- Case of List
        |   BinOp  BinaryOp Exp Exp  -- Binary operations
        |   UnOp   UnaryOp Exp       -- Unary operations
        deriving (Eq, Show)

data Value =    VUnit                      -- Unit value
        |       VInt    Integer            -- Integer value
        |       VClos   Value Exp          -- Closure
        |       VRcd    String Value       -- Single-field record value
        |       VMrg    Value Value        -- Merge of two values
        -- Extensions
        |       VBool   Bool               -- Boolean Value
        |       VString String             -- String Value
        |       VPair   Value Value        -- Pair value
        |       VInL    Typ Value          -- tagged value (left)
        |       VInR    Typ Value          -- tagged value (right)
        |       VNil    Typ                -- nil list
        |       VCons   Value Value        -- List
        deriving (Eq, Show)

data Typ =  TUnit                       -- Unit type for empty environment
        |   TInt                        -- Integer type
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
                      , Nil     <$> arbitrary
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
                      , Fix     <$> arbitrary <*> arbitrary
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
                        VInt    <$> arbitrary,
                        VNil    <$> arbitrary,
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