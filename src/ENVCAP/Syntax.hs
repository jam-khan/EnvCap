{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric #-}

module ENVCAP.Syntax where
import GHC.Generics (Generic)
import Data.Binary 

type Params             = [(String, SurfaceTyp)]
type Letargs            = [(String, SurfaceTyp, SurfaceTm)] 
type Name               = String
type Pattern            = (String, [String])
type Cases              = [(Pattern, SurfaceTm)]
type Import             = String
type Requirement        = String


data Parsed             = IMPL ParseImplData | INTF ParseIntfData

type ParseImplData      = (Name, Authority, Imports, Requirements, Statements)
type ParseIntfData      = (Name, Authority,          Requirements, Interface)

type Imports            = [Import]
type Requirements       = [Requirement]


type Interface          = [InterfaceStmt]

data InterfaceStmt      =   IAliasTyp       String SurfaceTyp
                        |   IAliasIntf      String Interface
                        |   FunctionIntf    Name Params SurfaceTyp
                        |   ModuleIntf      Name Params Interface
                        |   BindingTy       Name SurfaceTyp
                        |   IType           SurfaceTyp
                        |   IVar            String
                        deriving (Eq, Show)

type Statements         = [SurfaceTm]

data SurfaceTm          =   SCtx                                            -- ^ Query
                        |   SUnit                                           -- ^ Unit
                        |   SLit       Integer                              -- ^ Integer Literal
                        |   SBool      Bool                                 -- ^ Boolean Literal
                        |   SString    String                               -- ^ String  Literal
                        |   SVar       String                               -- ^ Variable
                        |   SOpen      SurfaceTm                            -- ^ Open
                        |   SLam       Params       SurfaceTm               -- ^ Abstraction with binding
                        |   SRec       String       SurfaceTm               -- ^ Record
                        |   SProj      SurfaceTm    Integer                 -- ^ Indexed lookup
                        |   SRProj     SurfaceTm    String                  -- ^ Labeled lookup
                        |   SApp       SurfaceTm    [SurfaceTm]             -- ^ Application
                        |   SMrg       [SurfaceTm]                          -- ^ Dependent Merge
                        |   STuple     [SurfaceTm]                          -- ^ Tuple
                        |   SWith      SurfaceTm    SurfaceTm               -- ^ With sandboxing
                        |   SStruct    Params       Statements              -- ^ First-class module
                        |   SLet       Letargs      SurfaceTm               -- ^ Let
                        |   SLetrec    Letargs      SurfaceTm               -- ^ Letrec
                        |   SIf        SurfaceTm    SurfaceTm SurfaceTm     -- ^ Conditional If
                        |   SADTInst   (String, [SurfaceTm])  SurfaceTyp    -- ^ Algebraic Data Type Instance
                        |   SCase      SurfaceTm    Cases                   -- ^ Pattern Matching
                        |   SSwitch    SurfaceTm    [(SurfaceTm, SurfaceTm)]-- ^ Switch coniditional
                        |   SCons      SurfaceTm    SurfaceTm               -- ^ List constructor
                        |   SList      [SurfaceTm]  SurfaceTyp              -- ^ List with type annotation
                        |   SBinOp     BinaryOp     SurfaceTm SurfaceTm     -- ^ Binary operation
                        |   SUnOp      UnaryOp      SurfaceTm               -- ^ Unary operation
                        |   SFunc      Name Params  SurfaceTyp SurfaceTm    -- ^ Function
                        |   SModule    Name Params  Interface Statements    -- ^ Module
                        |   SAliasTyp  String       SurfaceTyp              -- ^ Type Alis
                        |   SAliasIntf String       Interface               -- ^ Interface
                        |   SAnno      SurfaceTm    SurfaceTyp              -- ^ Type annotation
                        deriving (Eq, Show)

data SurfaceTyp         =   STUnit                              -- ^ Unit type for empty environment
                        |   STInt                               -- ^ Integer type
                        |   STBool                              -- ^ Boolean type
                        |   STString                            -- ^ String type
                        |   STAnd       SurfaceTyp SurfaceTyp   -- ^ Intersection type
                        |   STArrow     SurfaceTyp SurfaceTyp   -- ^ Arrow type, e.g. A -> B
                        |   STRecord    String     SurfaceTyp   -- ^ Single-Field Record Type
                        |   STUnion     SurfaceTyp SurfaceTyp   -- ^ Union
                        |   STList      SurfaceTyp              -- ^ Type for built-in list 
                        |   STSig       SurfaceTyp SurfaceTyp   -- ^ Sig Type End
                        |   STIden      String                  -- ^ Simply an alias
                        deriving (Eq, Show)

data SourceFragment     = TmFragment Name Authority SourceImport SourceRequirements SourceTyp
data Authority          = Pure | Resource 
                        deriving (Eq, Show)

data SourceHeader       = TmInterface Name Authority SourceRequirements SourceTyp
type SourceImport       = [(String, SourceHeader)]
type SourceRequirements = [(String, SourceHeader)]

data SourceTm           =   TmCtx                               -- Query
                        |   TmUnit                              -- Unit
                        |   TmLit       Integer                 -- Integer Literal
                        |   TmBool      Bool                    -- Boolean Literal
                        |   TmString    String                  -- String  Literal
                        |   TmLam       SourceTyp SourceTm      -- Abstraction with binding
                        |   TmClos      SourceTm SourceTyp SourceTm
                        |   TmRec       String SourceTm
                        |   TmRProj     SourceTm String
                        |   TmProj      SourceTm Integer        -- Projection on Expression
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

data SourceTyp          =   TySUnit                             -- Unit type for empty environment
                        |   TySInt                              -- Integer type
                        |   TySAnd      SourceTyp SourceTyp     -- Intersection type
                        |   TySArrow    SourceTyp SourceTyp     -- Arrow type, e.g. A -> B
                        |   TySRecord   String SourceTyp        -- Single-Field Record Type
                        |   TySBool                             -- Boolean type
                        |   TySString                           -- String type
                        |   TySList     SourceTyp               -- Type for built-in list 
                        |   TySUnion    SourceTyp SourceTyp     -- Type for sums
                        |   TySPair     SourceTyp SourceTyp     -- Type for pairs
                        |   TySSig      SourceTyp SourceTyp     -- Sig Type End
                        |   TySIden     String                  -- Simply an alias
                        deriving (Eq, Show)

data CoreTm             =   Ctx                                 -- Context
                        |   Unit                                -- Unit
                        |   EBool   Bool                        -- Boolean Term
                        |   EString String                      -- String Term
                        |   Lit    Integer                      -- Integer literal
                        |   Lam    CoreTyp CoreTm               -- Lambda Abstraction
                        |   Proj   CoreTm Integer               -- Projection
                        |   Clos   CoreTm  CoreTm               -- Closure
                        |   Rec    String CoreTm                -- Single-Field Record
                        |   RProj  CoreTm String                -- Record Projection by Label
                        |   App    CoreTm CoreTm                -- Application
                        |   Mrg    CoreTm CoreTm                -- Merge
                        |   Box    CoreTm CoreTm                -- Box
                        |   If     CoreTm CoreTm CoreTm         -- Conditionals
                        |   Fix    CoreTyp CoreTm               -- Recursion
                        |   Tag    CoreTm CoreTyp               -- Tagging for ADTs
                        |   Case   CoreTm [(Pattern, CoreTm)]   -- Pattern Matching
                        |   Nil    CoreTyp                      -- Nil List with Type
                        |   Cons   CoreTm CoreTm                -- List expression
                        |   LCase  CoreTm CoreTm CoreTm         -- 
                        |   BinOp  BinaryOp CoreTm CoreTm       -- Binary operations
                        |   UnOp   UnaryOp CoreTm               -- Unary operations
                        deriving (Eq, Show, Generic)

data CoreTyp            =   TyCUnit                       -- Unit type for empty environment
                        |   TyCInt                        -- Integer type
                        |   TyCAnd        CoreTyp CoreTyp -- Intersection type
                        |   TyCArrow      CoreTyp CoreTyp -- Arrow type, e.g. A -> B
                        |   TyCRecord     String  CoreTyp -- Single-Field Record Type
                        -- Extensions
                        |   TyCUnion      CoreTyp CoreTyp -- Union type
                        |   TyCList       CoreTyp         -- List Type
                        |   TyCBool                       -- Boolean type
                        |   TyCString                     -- String type
                        deriving (Eq, Show, Generic)

data Value              =       VUnit                      -- Unit value
                        |       VInt    Integer            -- Integer value
                        |       VClos   Value CoreTm       -- Closure
                        |       VRcd    String Value       -- Single-field record value
                        |       VMrg    Value Value        -- Merge of two values
                        -- Extensions
                        |       VNil    CoreTyp
                        |       VCons   Value Value
                        |       VTag    Value CoreTyp      
                        |       VBool   Bool               -- Boolean Value
                        |       VString String             -- String Value
                        deriving (Show, Eq, Generic)

instance Binary CoreTm
instance Binary CoreTyp
instance Binary Value
instance Binary BinaryOp
instance Binary UnaryOp
instance Binary ArithOp
instance Binary CompOp
instance Binary LogicOp

data UnaryOp    =       Not
                deriving (Eq, Show, Generic)

data BinaryOp   =       Arith ArithOp   -- Arithmetic
                |       Comp  CompOp    -- CompOp
                |       Logic LogicOp   -- Boolean Logic
                deriving (Eq, Show, Generic)

data ArithOp    = Add | Sub | Mul | Div | Mod
                deriving (Eq, Show, Generic)

data CompOp     = Eql | Neq | Lt | Le | Gt | Ge
                deriving (Eq, Show, Generic)

data LogicOp    = And | Or
                deriving (Eq, Show, Generic)