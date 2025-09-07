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
  | lrec   : String → Exp → Exp
  | rproj  : Exp → String → Exp
  deriving Repr

inductive IndexLookup : Typ -> Nat → Typ → Prop 
| zero (A B : Typ) : IndexLookup (Typ.and A B) 0 B
| succ (A B : Typ) (n : Nat) (C : Typ) : IndexLookup A n C → IndexLookup (Typ.and A B) (Nat.succ n) C

inductive LabelIn : String -> Typ -> Prop
| rcd (label : String) (T : Typ) : LabelIn label (Typ.rcd label T)
| andl (A B : Typ) (label : String) : LabelIn label A → LabelIn label (Typ.and A B)
| andr (A B : Typ) (label : String) : LabelIn label B → LabelIn label (Typ.and A B)

end LambdaE

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
  | lrec   : String → Exp → Exp
  | rproj  : Exp → String → Exp
  | slet   : Exp → Typ → Exp → Exp
  | sopen  : Exp → Exp → Exp
  deriving Repr

inductive IndexLookup : Typ → Nat → Typ → Prop
| zero (A B : Typ) : IndexLookup (Typ.and A B) 0 B
| succ (A B : Typ) (n : Nat) (C : Typ) : IndexLookup A n C → IndexLookup (Typ.and A B) (Nat.succ n) C

def ElaborateType : Typ → LambdaE.Typ
  | Typ.int        => LambdaE.Typ.int
  | Typ.top        => LambdaE.Typ.top
  | Typ.arr t1 t2  => LambdaE.Typ.arr (ElaborateType t1) (ElaborateType t2)
  | Typ.and t1 t2  => LambdaE.Typ.and (ElaborateType t1) (ElaborateType t2)
  | Typ.rcd str t  => LambdaE.Typ.rcd str (ElaborateType t)
  | Typ.sig t1 t2  => LambdaE.Typ.arr (ElaborateType t1) (ElaborateType t2)

inductive LabelIn : String -> Typ -> Prop
| rcd (label : String) (T : Typ) : LabelIn label (Typ.rcd label T)
| andl (A B : Typ) (label : String) : LabelIn label A → LabelIn label (Typ.and A B)
| andr (A B : Typ) (label : String) : LabelIn label B → LabelIn label (Typ.and A B)
end ENVCAP

theorem TypeElaborationUniqueness : ∀ (srcT : ENVCAP.Typ) (dstT1 dstT2 : LambdaE.Typ),
    ENVCAP.ElaborateType srcT = dstT1 →
    ENVCAP.ElaborateType srcT = dstT2 →
    dstT1 = dstT2 := by 
  intro srcT dstT1 dstT2 h1 h2;
  rw [← h1];
  rw [← h2];

theorem TypeSafeIndexLookup : ∀ (srcT1 : ENVCAP.Typ) (n : Nat) (srcT2 : ENVCAP.Typ),
    ENVCAP.IndexLookup srcT1 n srcT2 → 
    LambdaE.IndexLookup (ENVCAP.ElaborateType srcT1) n (ENVCAP.ElaborateType srcT2) := by
      intro srcT1 n srcT2 H;
      induction H with
      | zero A B =>
          apply LambdaE.IndexLookup.zero;
      | succ A B n C _ IH =>
          rw [ENVCAP.ElaborateType];
          apply LambdaE.IndexLookup.succ;  
          apply IH;
        
theorem TypeSafeLabelIn : ∀ (label : String) (srcT : ENVCAP.Typ),
    ENVCAP.LabelIn label srcT ↔ 
    LambdaE.LabelIn label (ENVCAP.ElaborateType srcT) := by
      intro label srcT;
      apply Iff.intro;
      · intro H;
        induction H with
        | rcd label T =>
            rw [ENVCAP.ElaborateType];
            apply LambdaE.LabelIn.rcd;
        | andl A B label _ IH =>
            rw [ENVCAP.ElaborateType];
            apply LambdaE.LabelIn.andl;
            apply IH;
        | andr A B label _ IH =>
            rw [ENVCAP.ElaborateType];
            apply LambdaE.LabelIn.andr;
            apply IH;
      · intro H;
        induction srcT with
        | rcd labelA T IH => 
            rw [ENVCAP.ElaborateType] at H;
            cases H
            apply ENVCAP.LabelIn.rcd
        | and A B IH1 IH2 => 
            rw [ENVCAP.ElaborateType] at H;
            cases H with
            | andl _ _ _ H1 =>
                apply ENVCAP.LabelIn.andl;
                apply IH1;
                apply H1;
            | andr _ _ _ H2 =>
                apply ENVCAP.LabelIn.andr;
                apply IH2;
                apply H2;
        | _ => cases H
