{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric #-}

module ENVCAP.Syntax where
import GHC.Generics (Generic)
import Data.Binary 

type Params             = [(String, SourceTyp)]
type Letargs            = [(String, SourceTyp, SourceTm)] 
type Name               = String

type SourceImports      = [(String, SourceInterface)]
type SourceRequirements = [(String, SourceInterface)]

data SourceFragment     = TmFragment Name Authority SourceImports SourceRequirements SourceInterface
data SourceInterface    =   STy             SourceTyp
                        |   STyFunc         Name SourceTyp
                        |   STyModule       Name SourceInterface
                        |   STyBind         Name SourceTyp
                        |   STyInterface    Name Authority SourceRequirements SourceInterface
                        deriving (Eq, Show)
data Authority          = Pure | Resource 
                        deriving (Eq, Show)

data SourceTm           =   TmCtx                               -- Query
                        |   TmUnit                              -- Unit
                        |   TmLit       Integer                 -- Integer Literal
                        |   TmBool      Bool                    -- Boolean Literal
                        |   TmString    String                  -- String  Literal
                        |   TmLam       Params SourceTm         -- Abstraction with binding
                        |   TmRec       String SourceTm         
                        |   TmRProj     SourceTm String
                        |   TmProj      SourceTm Integer        -- Projection on Expression
                        |   TmApp       SourceTm SourceTm
                        |   TmMrg       SourceTm SourceTm
                        |   TmWith      SourceTm SourceTm
                        -- To be elaborated
                        |   TmOpen      SourceTm SourceTm
                        |   TmSeq       SourceTm SourceTm
                        |   TmFunc      Params SourceTm
                        |   TmPair      SourceTm SourceTm
                        |   TmStruct    Params SourceTm
                        |   TmModule    Params SourceTm
                        -- Basic operation support
                        |   TmBinOp     BinaryOp SourceTm SourceTm
                        |   TmUnOp      UnaryOp SourceTm
                        deriving (Eq, Show)

data SourceTyp          =   TySUnit                             -- Unit type for empty environment
                        |   TySInt                              -- Integer type
                        |   TySAnd      SourceTyp SourceTyp     -- Intersection type
                        |   TySArrow    SourceTyp SourceTyp     -- Arrow type, e.g. A -> B
                        |   TySRecord   String SourceTyp        -- Single-Field Record Type
                        |   TySBool                             -- Boolean type
                        |   TySString                           -- String type
                        deriving (Eq, Show)

data CoreTm             =   Ctx                                 -- Context
                        |   EBool   Bool                        -- Boolean Term
                        |   EString String                      -- String Term
                        |   Unit                                -- Unit
                        |   Lit    Integer                      -- Integer literal
                        |   Lam    CoreTyp CoreTm               -- Lambda Abstraction
                        |   Proj   CoreTm Integer               -- Projection
                        |   Clos   CoreTm  CoreTm               -- Closure
                        |   Rec    String CoreTm                -- Single-Field Record
                        |   RProj  CoreTm String                -- Record Projection by Label
                        |   App    CoreTm CoreTm                -- Application
                        |   Mrg    CoreTm CoreTm                -- Merge
                        |   Box    CoreTm CoreTm                -- Box
                        |   BinOp  BinaryOp CoreTm CoreTm       -- Binary operations
                        |   UnOp   UnaryOp CoreTm               -- Unary operations
                        deriving (Eq, Show, Generic)

data CoreTyp            =   TyCUnit                       -- Unit type for empty environment
                        |   TyCBool                       -- Boolean type
                        |   TyCString                     -- String type
                        |   TyCInt                        -- Integer type
                        |   TyCAnd        CoreTyp CoreTyp -- Intersection type
                        |   TyCArrow      CoreTyp CoreTyp -- Arrow type, e.g. A -> B
                        |   TyCRecord     String  CoreTyp -- Single-Field Record Type
                        deriving (Eq, Show, Generic)

data Value              =       VUnit                      -- Unit value
                        |       VInt    Integer            -- Integer value
                        |       VBool   Bool               -- Boolean Value
                        |       VString String             -- String Value
                        |       VClos   Value CoreTm       -- Closure
                        |       VRcd    String Value       -- Single-field record value
                        |       VMrg    Value Value        -- Merge of two values
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