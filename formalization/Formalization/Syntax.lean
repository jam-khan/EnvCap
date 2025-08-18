
namespace ENVCAP

inductive Typ where
  | int  : Typ
  | top  : Typ
  | arr  : Typ → Typ → Typ
  | and  : Typ → Typ → Typ
  | rcd  : String → Typ → Typ
  | sig  : Typ → Typ → Typ
  deriving Repr

inductive Op where
  | app
  | withE
  | dmrg
  | ndmrg
  | mapp
  deriving Repr

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

namespace LambdaE

inductive Typ where
  | int  : Typ
  | top  : Typ
  | arr  : Typ → Typ → Typ
  | and  : Typ → Typ → Typ
  | rcd  : String → Typ → Typ
  deriving Repr

inductive Op where
  | app
  | box
  | mrg
  deriving Repr

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
