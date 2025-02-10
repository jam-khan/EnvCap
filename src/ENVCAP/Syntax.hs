{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module ENVCAP.Syntax where
import Test.QuickCheck ( Arbitrary(arbitrary), oneof, Gen)

-- Common between ASTs

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


data SurfaceTm 
        =   SCtx                               -- Query
        |   SUnit                              -- Unit
        |   SLit       Integer                 -- Integer Literal
        |   SBool      Bool                    -- Boolean Literal
        |   SString    String                  -- String  Literal
        |   SLam       SurfaceTyp SurfaceTm             -- Abstraction with binding
        |   SClos      SurfaceTm SurfaceTm SurfaceTm
        |   SRec       String SurfaceTm
        |   SRProj     SurfaceTm String
        |   SProj      SurfaceTm Int             -- Projection on Expression
        |   SApp       SurfaceTm SurfaceTm
        |   SMrg       SurfaceTm SurfaceTm
        |   SBox       SurfaceTm SurfaceTm
        -- Extension that require elaboration
        |   SIf        SurfaceTm SurfaceTm SurfaceTm
        |   SFix       SurfaceTm
        |   SPair      SurfaceTm SurfaceTm
        |   SFst       SurfaceTm
        |   SSnd       SurfaceTm
        |   SNil       SurfaceTyp
        |   SCons      SurfaceTm SurfaceTm
        |   SBinOp     BinaryOp SurfaceTm SurfaceTm
        |   SUnOp      UnaryOp SurfaceTm
        |   SCase      SurfaceTm
        |   SInL       SurfaceTm
        |   SInR       SurfaceTm
        |   SAnno      SurfaceTm SurfaceTyp  
        |   STuple     [SurfaceTm]
        |   SSwitch    SurfaceTm [(SurfaceTm, SurfaceTm)]
        |   SVar       String
        |   SStruct    SurfaceTyp SurfaceTm
        |   SFunc      String SurfaceTyp SurfaceTm
        |   SModule    String SurfaceTyp SurfaceTm
        |   SAliasTyp  String SurfaceTyp
        |   SLet       String SurfaceTyp SurfaceTm SurfaceTm
        |   SLetrec    String SurfaceTyp SurfaceTm SurfaceTm
        deriving (Eq, Show)

-- Types
data SurfaceTyp =   STUnit                  -- Unit type for empty environment
                |   STInt                   -- Integer type
                |   STAnd SurfaceTyp SurfaceTyp           -- Intersection type
                |   STArrow SurfaceTyp SurfaceTyp         -- Arrow type, e.g. A -> B
                |   STRecord String SurfaceTyp     -- Single-Field Record Type
                -- Extensions
                |   STBool                  -- Boolean type
                |   STString                -- String type
                |   STList  SurfaceTyp             -- Type for built-in list 
                |   STSum   SurfaceTyp SurfaceTyp         -- Type for sums
                |   STPair  SurfaceTyp SurfaceTyp
                |   STSig   SurfaceTyp SurfaceTyp             -- Sig Type End
                |   STIden  String          -- Simply an alias
                deriving (Eq, Show)

data Tm =   TmCtx                               -- Query
        |   TmUnit                              -- Unit
        |   TmLit       Integer                 -- Integer Literal
        |   TmBool      Bool                    -- Boolean Literal
        |   TmString    String                  -- String  Literal
        |   TmLam       TypS Tm                 -- Abstraction with binding
        |   TmClos      Tm TypS Tm
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
        |   TmNil       TypS
        |   TmCons      Tm Tm
        |   TmBinOp     BinaryOp Tm Tm
        |   TmUnOp      UnaryOp Tm
        |   TmCase      Tm
        |   TmInL       Tm
        |   TmInR       Tm
        -- Not sure if tagging is needed at source level -- can be simply added during elaboration to core
        |   TmAnno      Tm TypS
        |   TmTuple     [Tm]
        |   TmSwitch    Tm [(Tm, Tm)]
        |   TmVar       String
        |   TmStruct    TypS Tm
        |   TmFunc      String TypS Tm
        |   TmModule    String TypS Tm
        |   TmAliasTyp  String TypS
        |   TmLet       String TypS Tm Tm
        |   TmLetrec    String TypS Tm Tm
        deriving (Eq, Show)

-- Types
data TypS     =     TySUnit                  -- Unit type for empty environment
                |   TySInt                   -- Integer type
                |   TySAnd      TypS TypS           -- Intersection type
                |   TySArrow    TypS TypS         -- Arrow type, e.g. A -> B
                |   TySRecord   String TypS     -- Single-Field Record Type
                |   TySBool                  -- Boolean type
                |   TySString                -- String type
                |   TySList     TypS             -- Type for built-in list 
                |   TySSum      TypS TypS         -- Type for sums
                |   TySPair     TypS TypS         -- Type for pairs
                |   TySSig      TypS TypS         -- Sig Type End
                |   TySIden     String          -- Simply an alias
                deriving (Eq, Show)


data Exp =  Ctx                      -- Context
        |   Unit                     -- Unit
        |   Lit    Integer           -- Integer literal
        |   Lam    TypC Exp           -- Lambda Abstraction
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
        |   Fix    TypC Exp           -- Recursion
        |   Pair   Exp Exp           -- Pair
        |   Fst    Exp               -- First Projection
        |   Snd    Exp               -- Second Projection
        |   InL    TypC Exp           -- Tagging Left
        |   InR    TypC Exp           -- Tagging Right
        |   Case   Exp Exp Exp       -- Case of Sums
        |   Nil    TypC               -- Nil typ, e.g. [] of Int
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
        |       VInL    TypC Value          -- tagged value (left)
        |       VInR    TypC Value          -- tagged value (right)
        |       VNil    TypC                -- nil list
        |       VCons   Value Value        -- List
        deriving (Eq, Show)

data TypC 
        =   TyCUnit                       -- Unit type for empty environment
        |   TyCInt                        -- Integer type
        |   TyCAnd        TypC TypC         -- Intersection type
        |   TyCArrow      TypC TypC         -- Arrow type, e.g. A -> B
        |   TyCRecord     String TypC     -- Single-Field Record Type
        -- Extensions
        |   TyCBool                       -- Boolean type
        |   TyCString                     -- String type
        |   TyCList       TypC             -- Type for built-in list
        |   TyCSum        TypC TypC         -- Type for sums
        |   TyCPair       TypC TypC
        deriving (Eq, Show)


instance Arbitrary TypC where
    arbitrary :: Gen TypC
    arbitrary = oneof [ return TyCUnit,
                        return TyCInt,
                        return TyCBool,
                        return TyCString,
                        TyCAnd    <$> arbitrary <*> arbitrary,
                        TyCArrow  <$> arbitrary <*> arbitrary,
                        TyCRecord <$> arbitrary <*> arbitrary,
                        TyCList   <$> arbitrary,
                        TyCSum    <$> arbitrary <*> arbitrary,
                        TyCPair   <$> arbitrary <*> arbitrary]

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
