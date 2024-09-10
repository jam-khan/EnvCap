module EnvCap.Core where
import qualified Data.Bool as e
-- Core LamdaE Calculus Representation

-- LamdaE Calculus 
-- Specifications and Implementation Details

-- ** Syntax **

-- Types            A, B, L ::= Int | eps | A & B | A -> B | {l : A}
-- Expressions            e ::= ? | e.n | i | eps | lamda A . e | e1 |> e2 
--                                | <v, lamda A . e> | e1 e2 | e1 merge e2 | {l = e} | e.l
-- Values                 v ::= i | eps | <v, lambda A . e> | v1 merge v2 | {l = v}
-- Frames                 F ::= [].n | [] merge e| [] e | [] |> e | v[] | {l = []} | [].l
-- Syntactic Sugar      _n_ ::= ?.n
--                      _l_ ::= ?.l

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