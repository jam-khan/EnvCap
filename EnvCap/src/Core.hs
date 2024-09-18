module EnvCap.Core where
-- Core LamdaE Calculus Representation

-- LamdaE Calculus 

data Typ = 
        TInt                    -- Integer type
    |   TEmpty                  -- Unit type for empty environment
    |   TAnd Typ Typ            -- Intersection type
    |   TArrow Typ Typ          -- Arrow type, e.g. A -> B
    |   TRecord { label :: String, typeVal :: Typ } -- Single-Field Record Type
    deriving (Eq, Show)


-- ******* Potential Improvement? ********
data LookupResult = Found Typ | NotFound
    deriving (Eq, Show)

-- Look up function for Types
lookupType :: Typ -> Int -> LookupResult
lookupType (TAnd left right) n
    | n == 0 = Found left
    | n == 1 = Found right
    | otherwise = case lookupType left (n - 1) of
                    Found foundType -> Found foundType
                    NotFound        -> lookupType right (n - 1)
lookupType _ _ = NotFound

-- ******* Testing ********
-- ******* Potential Improvement? ********

-- Operations Definitions
data Op =   App -- Application
        |   Box -- Box
        |   Mrg -- Merge
        deriving (Eq, Show)

data Expr = Ctx                     -- Context
        |   Unit                    -- Unit
        |   Lit Int                 -- Integer literal
        |   BinOp Op Expr Expr      -- Binary operations: Application, Box and Merge
        |   Lam Typ Expr            -- Lambda Abstraction
        |   Proj Expr Int           -- Projection
        |   Clos Expr Typ Expr      -- Closure
        |   Rec  String Expr        -- Single-Field Record
        |   RProj Expr String       -- Record Projection by Label
        deriving (Eq, Show)


-- Values
data Value = VInt                    -- Integer value
        |    VUnit                   -- Unit value
        |    VClos Value Typ Expr    -- Closure
        |    VRcd String Value       -- Single-field record value
        |    VMrg Value Value        -- Merge of two values
        deriving (Eq, Show)


isValue :: Expr -> Bool
isValue (Lit _)             = True
isValue Unit                = True
isValue (Clos v _ _)        = isValue v
isValue (Rec _ v)           = isValue v 
isValue (BinOp Mrg v1 v2)   = isValue v1 && isValue v2
isValue _                   = False

-- Specifications and Implementation Details

-- ** Syntax **

-- Types    
-- A, B, L ::= Int | Empty | A & B | A -> B | {l : A}

-- **** Types and contexts ****

--  Meta-variables A, B, L range over types and typing contexts 
--      (No distinction between types and contexts)
--      Contexts are types and any type can act as a context
--      Int     : base type
--      eps     : unit type for empty environment
--      A & B   : limited form of intersection types
--      A -> B  : arrow types
--      {l : A} : Single-field record type

-- Note: multi-fied record types can be obtained
--      {l1 : A1, ..., ln : An} obtained by {l1 : A1} & ... & {ln : An}

-- Expressions            e ::= ? | e.n | i | eps | lamda A . e | e1 |> e2 
--                                | <v, lamda A . e> | e1 e2 | e1 merge e2 | {l = e} | e.l
-- **** Expressions ****

-- Meta-variables e range over expressions
--      i                   : integers
--      eps                 : unit expression
--      e1 e2               : applications
--      e1 merge e2         : merges (left associative, e.g. e1 merge e2 merge e3 = (e1 merge e2) merge e3 )
--      lambda A . e        : lambda abstraction (A is nameless due to de Bruijn notation)
--      e1 |> e2            : boxes where e1 evaluates to a value v1 and then, e2 evaluates to v2 under environment v1
--      <v, lamda A . e>    : closure which records lexical environment v of a function
--      e.n                 : returns the nth element of an expression
--      ?.n                 : What does it do???????????????????
--      {l = e}             : Single-field record
--      e.l                 : lookup by label (possible to have labelled entries in the environment)


-- We want to be able to Lookup Type

-- Values                 v ::= i | eps | <v, lambda A . e> | v1 merge v2 | {l = v}
-- Frames                 F ::= [].n | [] merge e| [] e | [] |> e | v[] | {l = []} | [].l
-- Syntactic Sugar      _n_ ::= ?.n
--                      _l_ ::= ?.l

-- **** Values and frames ****

-- Meta-variables v range over values.
-- A value is either of the following:
--      i                   : integer
--      eps                 : unit expression
--      <v, lambda A . e>   : closure where lexical environment v is also a value
--      v1 merge v2         : merge of two values
--      {l = v}             : record with a value field

-- Frames F include
--      [].n        : projection
--      [] merge e  : merges
--      [] e        : application
--      [] |> e     : boxes
--      []  
--  F ::= [].n | [] merge e| [] e | [] |> e | v[] | {l = []} | [].l

-- Note: {l1 = e1, ..., ln = en} = {l1 = e1} merge ... merge {ln = en}



