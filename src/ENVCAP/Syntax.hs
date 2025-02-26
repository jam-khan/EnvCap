{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module ENVCAP.Syntax where

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


type Params       = [(String, SurfaceTyp)]
type Letargs      = [(String, SurfaceTyp, SurfaceTm)] 
type Name         = String
type Pattern      = (String, [String])
type Cases        = [(Pattern, SurfaceTm)]

data Interface  
        =       IAliasTyp       String SurfaceTyp
        |       IType           SurfaceTyp
        |       FunctionTyp     Name Params SurfaceTyp
        |       ModuleTyp       Name Params SurfaceTyp
        |       Binding         Name SurfaceTyp
        |       InterfaceAnd    Interface Interface
        deriving (Eq, Show)

data SurfaceTm 
        =   SCtx                                        -- Query
        |   SUnit                                       -- Unit
        |   SLit       Integer                          -- Integer Literal
        |   SBool      Bool                             -- Boolean Literal
        |   SString    String                           -- String  Literal
        |   SLam       Params SurfaceTm                   -- Abstraction with binding
        |   SClos      SurfaceTm Params SurfaceTm
        |   SRec       String SurfaceTm
        |   SRProj     SurfaceTm String
        |   SProj      SurfaceTm Integer                -- Projection on Expression
        |   SApp       SurfaceTm [SurfaceTm]
        |   SMrg       SurfaceTm SurfaceTm
        |   SBox       SurfaceTm SurfaceTm
        |   SVar       String
        |   SStruct    Params SurfaceTm
        |   SFunc      Name Params SurfaceTyp SurfaceTm
        |   SModule    Name Params SurfaceTm
        |   SAliasTyp  String SurfaceTyp
        |   SLet       Letargs SurfaceTm
        |   SLetrec    Letargs SurfaceTm
        |   SBinOp     BinaryOp SurfaceTm SurfaceTm
        |   SUnOp      UnaryOp SurfaceTm
        |   SAnno      SurfaceTm SurfaceTyp
        |   SIf        SurfaceTm SurfaceTm SurfaceTm
        -- Extra parts
        |   SPair      SurfaceTm SurfaceTm
        |   SFst       SurfaceTm
        |   SSnd       SurfaceTm
        |   SNil       SurfaceTyp
        |   SCons      SurfaceTm SurfaceTm
        |   STuple     [SurfaceTm]
        |   SSwitch    SurfaceTm [(SurfaceTm, SurfaceTm)]
        |   SADTInst   (String, [SurfaceTm]) SurfaceTyp 
        -- Cases = [(Pattern, SurfaceTm)]
        |   SCase      SurfaceTm Cases
        deriving (Eq, Show)

-- Types
data SurfaceTyp =   STUnit                              -- Unit type for empty environment
                |   STInt                               -- Integer type
                |   STAnd SurfaceTyp SurfaceTyp         -- Intersection type
                |   STArrow SurfaceTyp SurfaceTyp       -- Arrow type, e.g. A -> B
                |   STRecord String SurfaceTyp          -- Single-Field Record Type
                -- Extensions
                |   STBool                              -- Boolean type
                |   STString                            -- String type
                |   STList  SurfaceTyp                  -- Type for built-in list 
                |   STSum   SurfaceTyp SurfaceTyp       -- Type for sums
                |   STPair  SurfaceTyp SurfaceTyp
                |   STSig   SurfaceTyp SurfaceTyp       -- Sig Type End
                |   STIden  String                      -- Simply an alias
                deriving (Eq, Show)

data SourceTm   =   TmCtx                               -- Query
                |   TmUnit                              -- Unit
                |   TmLit       Integer                 -- Integer Literal
                |   TmBool      Bool                    -- Boolean Literal
                |   TmString    String                  -- String  Literal
                |   TmLam       SourceTyp SourceTm                 -- Abstraction with binding
                |   TmClos      SourceTm SourceTyp SourceTm
                |   TmRec       String SourceTm
                |   TmRProj     SourceTm String
                |   TmProj      SourceTm Integer                  -- Projection on Expression
                |   TmApp       SourceTm SourceTm
                |   TmMrg       SourceTm SourceTm
                |   TmBox       SourceTm SourceTm
                -- Extension that require elaboration
                |   TmIf        SourceTm SourceTm SourceTm
                |   TmFix       SourceTyp SourceTm
                |   TmNil       SourceTyp
                |   TmCons      SourceTm SourceTm
                -- [SourceTm] should become merges
                |   TmTag       SourceTm SourceTyp
                |   TmCase      SourceTm [(Pattern, SourceTm)]
                |   TmBinOp     BinaryOp SourceTm SourceTm
                |   TmUnOp      UnaryOp SourceTm
                -- Not sure if tagging is needed at source level -- can be simply added during elaboration to core
                |   TmAnno      SourceTm SourceTyp
                |   TmTuple     [SourceTm]
                |   TmStruct    SourceTyp SourceTm
                |   TmModule    String SourceTyp SourceTm
                deriving (Eq, Show)

-- Types
data SourceTyp  =   TySUnit                             -- Unit type for empty environment
                |   TySInt                              -- Integer type
                |   TySAnd      SourceTyp SourceTyp     -- Intersection type
                |   TySArrow    SourceTyp SourceTyp     -- Arrow type, e.g. A -> B
                |   TySRecord   String SourceTyp        -- Single-Field Record Type
                |   TySBool                             -- Boolean type
                |   TySString                           -- String type
                |   TySList     SourceTyp               -- Type for built-in list 
                |   TySSum      SourceTyp SourceTyp     -- Type for sums
                |   TySPair     SourceTyp SourceTyp     -- Type for pairs
                |   TySSig      SourceTyp SourceTyp     -- Sig Type End
                |   TySIden     String                  -- Simply an alias
                deriving (Eq, Show)

data CoreTm     =   Ctx                                 -- Context
                |   Unit                                -- Unit
                |   Lit    Integer                      -- Integer literal
                |   Lam    CoreTyp CoreTm               -- Lambda Abstraction
                |   Proj   CoreTm Integer                   -- Projection
                |   Clos   CoreTm  CoreTm               -- Closure
                |   Rec    String CoreTm                -- Single-Field Record
                |   RProj  CoreTm String                -- Record Projection by Label
                |   App    CoreTm CoreTm                -- Application
                |   Mrg    CoreTm CoreTm                -- Merge
                |   Box    CoreTm CoreTm                -- Box
                -- Extensions
                |   EBool   Bool                        -- Boolean Term
                |   EString String                      -- String Term
                |   If     CoreTm CoreTm CoreTm         -- Conditionals
                |   Fix    CoreTyp CoreTm               -- Recursion
                |   Tag    CoreTm CoreTyp
                |   Case   CoreTm [(Pattern, CoreTm)]
                |   BinOp  BinaryOp CoreTm CoreTm       -- Binary operations
                |   UnOp   UnaryOp CoreTm               -- Unary operations
                deriving (Eq, Show)

data CoreTyp    =   TyCUnit                       -- Unit type for empty environment
                |   TyCInt                        -- Integer type
                |   TyCAnd        CoreTyp CoreTyp -- Intersection type
                |   TyCArrow      CoreTyp CoreTyp -- Arrow type, e.g. A -> B
                |   TyCRecord     String  CoreTyp -- Single-Field Record Type
                -- Extensions
                |   TyCBool                       -- Boolean type
                |   TyCString                     -- String type
                |   TyCList       CoreTyp         -- Type for built-in list
                |   TyCSum        CoreTyp CoreTyp -- Type for sums
                |   TyCPair       CoreTyp CoreTyp
                deriving (Eq, Show)

data Value =    VUnit                      -- Unit value
        |       VInt    Integer            -- Integer value
        |       VClos   Value CoreTm          -- Closure
        |       VRcd    String Value       -- Single-field record value
        |       VMrg    Value Value        -- Merge of two values
        -- Extensions
        |       VBool   Bool               -- Boolean Value
        |       VString String             -- String Value
        |       VPair   Value Value        -- Pair value
        |       VInL    CoreTyp Value      -- tagged value (left)
        |       VInR    CoreTyp Value      -- tagged value (right)
        |       VNil    CoreTyp            -- nil list
        |       VCons   Value Value        -- List
        deriving (Show, Eq)
