Require Import LibTactics.
Require Import Arith.
Require Import Lia.
From Coq Require Import Strings.String.
Require Import Coq.Lists.List.
Require Import Coq.Classes.EquivDec.


Inductive styp :=
  | Sint : styp
  | Stop : styp
  | Sarr : styp -> styp -> styp
  | Sand : styp -> styp -> styp
  | Srcd : string -> styp -> styp
  | Ssig : styp -> styp -> styp.

Inductive sop := Sapp.

Inductive sexp :=
  | Sctx        : sexp  
  | Sunit       : sexp
  | Slit        : nat   -> sexp
  | Sbinop      : sop   -> sexp -> sexp -> sexp
  | Slam        : styp  -> sexp -> sexp.


Inductive typ :=
  | int : typ
  | top : typ
  | arr : typ -> typ -> typ
  | and : typ -> typ -> typ
  | rcd : string -> typ -> typ.

Inductive elaborate_typ : styp -> typ -> Prop :=
  | elint : elaborate_typ Sint int
  | eltop : elaborate_typ Stop top
  | elarr : forall A A' B B',
            elaborate_typ A A' ->
            elaborate_typ B B' ->
            elaborate_typ (Sarr A B) (arr A' B')
  | eland : forall A A' B B',
            elaborate_typ A A' ->
            elaborate_typ B B' ->
            elaborate_typ (Sand A B) (and A' B')
  | elrcd : forall l A A',
            elaborate_typ A A' ->
            elaborate_typ (Srcd l A) (rcd l A')
  | elsig : forall A A' B B',
            elaborate_typ A A' ->
            elaborate_typ B B' ->
            elaborate_typ (Ssig A B) (arr A' B').

Inductive op := app | box | mrg.

Inductive exp :=
  | ctx         : exp
  | lit         : nat -> exp
  | unit        : exp
  | binop       : op -> exp -> exp -> exp
  | lam         : typ -> exp -> exp
  | proj        : exp -> nat -> exp
  | clos        : exp -> typ -> exp -> exp
  | rec         : string -> exp -> exp
  | rproj       : exp -> string -> exp.

Inductive elaborate_sexp : styp -> sexp -> styp -> exp -> Prop :=
  | infctx: forall E, 
        elaborate_sexp E Sctx E ctx
  | infint: forall E n,
        elaborate_sexp E (Slit n) Sint (lit n)
  | infunit: forall E,
        elaborate_sexp E Sunit Stop unit
  | inflam: forall E A A' Se e B,
      elaborate_typ A A' ->
      elaborate_sexp (Sand E A) Se B e ->
      elaborate_sexp E (Slam A Se) (Sarr A B) (lam A' e)
(*  | tapp : forall E A B e1 e2,
      has_type E e1 (arr A B) -> has_type E e2 A -> has_type E (binop app e1 e2) B *)
  | infapp: forall E A B sE1 sE2 cE1 cE2,
      elaborate_sexp E sE2 A cE2 ->
      elaborate_sexp E sE1 (Sarr A B) cE1 ->
      elaborate_sexp E (Sbinop Sapp sE1 sE2) B (binop app cE1 cE2).
  
Inductive lookup : typ -> nat -> typ -> Prop :=
  | lzero : forall A B, 
      lookup (and A B) 0 B
  | lsucc : forall A B n C, 
      lookup A n C -> 
      lookup (and A B) (S n) C.

Inductive value : exp -> Prop :=
  | vint    : forall n, value (lit n)
  | vunit   : value unit
  | vclos   : forall v A e, value v -> value (clos v A e)
  | vrcd    : forall l v, value v -> value (rec l v)
  | vmrg    : forall v1 v2, value v1 -> value v2 -> value (binop mrg v1 v2).

Inductive lin: string -> typ -> Prop :=
  | lin_rcd: forall A l, lin l (rcd l A)
  | lin_andl: forall A B l,
      lin l A ->
      lin l (and A B)
  | lin_andr: forall A B l,
     lin l B ->
     lin l (and A B).

Inductive rlookup : typ -> string -> typ -> Prop :=
  | rlzero : forall l B, 
      rlookup (rcd l B) l B
  | landl : forall A B C l, 
      rlookup A l C ->
      ~ lin l B ->
      rlookup (and A B) l C
  | landr : forall A B C l, 
      rlookup B l C ->
      ~ lin l A ->
      rlookup (and A B) l C.

Inductive has_type : typ -> exp -> typ -> Prop :=
  | tctx: forall E, has_type E ctx E
  | tint : forall E i, has_type E (lit i) int
  | tunit : forall E, has_type E unit top
  | tapp : forall E A B e1 e2,
      has_type E e1 (arr A B) -> has_type E e2 A -> has_type E (binop app e1 e2) B
  | tbox : forall E A E1 e1 e2,
      has_type E e1 E1 -> has_type E1 e2 A -> has_type E (binop box e1 e2) A
  | tmrg : forall E e1 e2 A B,
      has_type E e1 A -> has_type (and E A) e2 B -> has_type E (binop mrg e1 e2) (and A B)
  | tlam : forall E A e B,
      has_type (and E A) e B -> has_type E (lam A e) (arr A B)
  | tproj: forall E A e B n,
      has_type E e A ->
      lookup A n B ->
      has_type E (proj e n) B
  | tclos : forall E E1 A e ev B, 
      value ev ->
      has_type top ev E1 -> 
      has_type (and E1 A) e B -> 
      has_type E (clos ev A e) (arr A B)
  | trcd : forall E e A l,
      has_type E e A ->
      has_type E (rec l e) (rcd l A)
  | trproj : forall E e B l A,
      has_type E e B ->
      rlookup B l A ->
      has_type E (rproj e l) A.

Inductive lookupv : exp -> nat -> exp -> Prop :=
  | lvzero : forall v1 v2, 
      lookupv (binop mrg v1 v2) 0 v2
  | lvsucc : forall v1 v2 n v3, 
      lookupv v1 n v3 -> 
      lookupv (binop mrg v1 v2) (S n) v3.

Inductive rlookupv : exp -> string -> exp -> Prop :=
  | rvlzero : forall l e, 
      rlookupv (rec l e) l e
  | vlandl : forall e1 e2 e l, 
      rlookupv e1 l e ->
      rlookupv (binop mrg e1 e2) l e
  | vlandr : forall e1 e2 e l, 
      rlookupv e2 l e ->
      rlookupv (binop mrg e1 e2) l e.

Inductive step : exp -> exp -> exp -> Prop :=
  | sctx: forall v,
      value v ->
      step v ctx v
  | sbl : forall v e1 e2 e1' o,
      value v ->
      step v e1 e1' -> 
      step v (binop o e1 e2) (binop o e1' e2)
  | sbapp : forall v v1 e2 e2', 
      value v -> 
      value v1 -> 
      step v e2 e2' -> 
      step v (binop app v1 e2) (binop app v1 e2')
  | sbbox : forall v v1 e2 e2', 
      value v -> 
      value v1 -> 
      step v1 e2 e2' -> 
      step v (binop box v1 e2) (binop box v1 e2')
  | sbmrg : forall v v1 e' e,
      value v -> 
      value v1 -> 
      step (binop mrg v v1) e e' -> 
      step v (binop mrg v1 e) (binop mrg v1 e')
  | sclos : forall v A e, 
      value v -> 
      step v (lam A e) (clos v A e)
  | sbeta : forall v v1 v2 A e, 
      value v -> 
      value v1 ->
      value v2 -> 
      step v (binop app (clos v2 A e) v1) (binop box (binop mrg v2 v1) e)
  | sboxv : forall v v1 v2,
      value v -> 
      value v1 -> 
      value v2 -> 
      step v (binop box v1 v2) v2
  | sproj: forall v e1 e2 n,
      value v ->
      step v e1 e2 ->
      step v (proj e1 n) (proj e2 n)
  | sprojv : forall v n v1 v2, 
      value v ->
      value v1 ->
      lookupv v1 n v2 ->
      step v (proj v1 n) v2
  | srec: forall v e1 e2 l,
      value v ->
      step v e1 e2 ->
      step v (rec l e1) (rec l e2)
  | srproj: forall v e1 e2 l,
      value v ->
      step v e1 e2 ->
      step v (rproj e1 l) (rproj e2 l)
  | srprojv : forall v l v1 v2, 
      value v ->
      value v1 ->
      rlookupv v1 l v2 ->
      step v (rproj v1 l) v2.

#[export]
Hint Constructors typ op exp lookup lookupv lin rlookup styp sop sexp elaborate_typ elaborate_sexp has_type value rlookupv step : core.


Require Import Program.Equality.

Lemma type_elaboration_unique : forall E E' E'',
  elaborate_typ E E' ->
  elaborate_typ E E'' ->
  E' = E''.
Proof.
  intros E E' E'' H1 H2.
  generalize dependent E''.
  induction H1; intros E'' H2; try inversion H2; subst; try reflexivity;
    try ( apply IHelaborate_typ1 in H1;
          apply IHelaborate_typ2 in H4;
          rewrite H1; rewrite H4; reflexivity).
  + apply IHelaborate_typ in H4.
    rewrite H4. reflexivity.
Qed.

(* ---------------------------------------------------- *)
(* Elaboration *)
Lemma type_safe_translation : forall E E' SE A A' CE,
  elaborate_typ E E' ->
  elaborate_typ A A' ->
  elaborate_sexp E SE A CE ->
  has_type E' CE A'.
Proof.
  intros E E' SE A A' CE HA HE H.
  generalize dependent A'.
  generalize dependent E'.
  induction H.
  
  induction H; subst; exists; intros HE HA.
  + apply tctx.
  + apply tint.
  + apply tunit.
  + apply tlam.
Qed.
