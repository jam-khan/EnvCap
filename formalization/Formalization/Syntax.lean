
/-!
# ENVCAP Language

This module defines the syntax of the ENVCAP calculus.
-/

namespace ENVCAP

/-- Types in the ENVCAP language. -/
inductive Typ where
  | int  : Typ
  | top  : Typ
  | arr  : Typ → Typ → Typ
  | and  : Typ → Typ → Typ
  | rcd  : String → Typ → Typ
  | sig  : Typ → Typ → Typ
  deriving Repr

/-- Binary operators in the ENVCAP language. -/
inductive Op where
  | app
  | withE
  | dmrg
  | ndmrg
  | mapp
  deriving Repr

/-- Expressions in the ENVCAP language. -/
inductive Exp where
  | ctx    : Exp
  | unit   : Exp
  | lit    : Nat → Exp
  | binop  : Op → Exp → Exp → Exp
  | lam    : Typ → Exp → Exp
  | proj   : Exp → Nat → Exp
  | clos   : Exp → Typ → Exp → Exp
  | struct : Typ → Exp → Exp
  | lrec    : String → Exp → Exp
  | rproj  : Exp → String → Exp
  | slet    : Exp → Typ → Exp → Exp
  | sopen   : Exp → Exp → Exp
  deriving Repr

end ENVCAP

/-!
# LambdaE Calculus

This module defines the syntax of the λE calculus.
-/
namespace LambdaE

/--
Types in the λE calculus.
-/
inductive Typ where
  | int  : Typ
  | top  : Typ
  | arr  : Typ → Typ → Typ
  | and  : Typ → Typ → Typ
  | rcd  : String → Typ → Typ
  deriving Repr

/--
Binary operators in λE.
-/
inductive Op where
  | app
  | box
  | mrg
  deriving Repr

/--
Expressions in the λE.
-/
inductive Exp where
  | ctx    : Exp
  | lit    : Nat → Exp
  | unit   : Exp
  | binop  : Op → Exp → Exp → Exp
  | lam    : Typ → Exp → Exp
  | proj   : Exp → Nat → Exp
  | clos   : Exp → Typ → Exp → Exp
  | lrec    : String → Exp → Exp
  | rproj  : Exp → String → Exp
  deriving Repr

end LambdaE
