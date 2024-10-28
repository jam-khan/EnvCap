Require Import LibTactics.
Require Import Arith.
Require Import Lia.
From Coq Require Import Strings.String.
Require Import Coq.Lists.List.
Require Import Coq.Classes.EquivDec.


Inductive typ :=
  | int : typ
  | top : typ
  | arr : typ -> typ -> typ
  | and : typ -> typ -> typ
  | rcd : string -> typ -> typ.

Inductive op := app | box | mrg.

Inductive exp :=
  | ctx : exp
  | lit : nat -> exp
  | unit : exp
  | binop : op -> exp -> exp -> exp 
  | lam : typ -> exp -> exp
  | proj: exp -> nat -> exp
  | clos: exp -> typ -> exp -> exp
  | rec : string -> exp -> exp
  | rproj : exp -> string -> exp.

Inductive lookup : typ -> nat -> typ -> Prop :=
  | lzero : forall A B, 
      lookup (and A B) 0 B
  | lsucc : forall A B n C, 
      lookup A n C -> 
      lookup (and A B) (S n) C.

Inductive value : exp -> Prop :=
  | vint : forall n, value (lit n)
  | vunit : value unit
  | vclos : forall v A e, value v -> value (clos v A e)
  | vrcd : forall l v, value v -> value (rec l v)
  | vmrg : forall v1 v2, value v1 -> value v2 -> value (binop mrg v1 v2).

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

Inductive mode := inf | chk.

Inductive has_type2 : typ -> exp -> mode -> typ -> Prop :=
  | tctx2: forall E, has_type2 E ctx inf E
  | tint2 : forall E i, has_type2 E (lit i) inf int
  | tunit2 : forall E, has_type2 E unit inf top
  | tapp2 : forall E A B e1 e2,
      has_type2 E e1 inf (arr A B) -> has_type2 E e2 chk A -> has_type2 E (binop app e1 e2) inf B
  | tbox2 : forall E A E1 e1 e2,
      has_type2 E e1 inf E1 -> has_type2 E1 e2 inf A -> has_type2 E (binop box e1 e2) inf A
  | tmrg2 : forall E e1 e2 A B,
      has_type2 E e1 inf A -> has_type2 (and E A) e2 inf B -> has_type2 E (binop mrg e1 e2) inf (and A B)
  | tlam2 : forall E A e B,
      has_type2 (and E A) e chk B -> has_type2 E (lam A e) inf (arr A B)
  | tproj2: forall E A e B n,
      has_type2 E e inf A ->
      lookup A n B ->
      has_type2 E (proj e n) inf B
  | tclos2 : forall E E1 A e ev B, 
      value ev ->
      has_type2 top ev inf E1 -> 
      has_type2 (and E1 A) e chk B -> 
      has_type2 E (clos ev A e) inf (arr A B)
  | trcd2 : forall E e A l,
      has_type2 E e inf A ->
      has_type2 E (rec l e) inf (rcd l A)
  | trproj2 : forall E e B l A,
      has_type2 E e inf B ->
      rlookup B l A ->
      has_type2 E (rproj e l) inf A
  | teq: forall E e A B,
      has_type2 E e inf A ->
      A = B ->
      has_type2 E e chk B.

#[export]
Hint Constructors typ op exp lookup has_type has_type2 lin rlookup value mode: core.

Lemma bi_sound: forall E e m A,
  has_type2 E e m A ->
  has_type E e A.
Proof.
  introv Ht. induction Ht; eauto.
  - subst. eauto. 
Qed.

Lemma chk_inf: forall E e m A,
  has_type2 E e m A -> 
  has_type2 E e inf A.
Proof.
  introv Ht. inductions Ht; eauto.
  - subst. eauto. 
Qed.

Lemma bi_complete: forall E e A,
  has_type E e A -> exists m,
  has_type2 E e m A.
Proof.
  introv Ht. inductions Ht; try solve [eauto].
  - inverts* IHHt1. inverts* IHHt2. 
    destruct* x; destruct* x0.
    + forwards~: chk_inf H. eauto.
    + forwards~: chk_inf H. forwards~: chk_inf H0. eauto.
  - inverts* IHHt1. inverts* IHHt2. 
    destruct* x; destruct* x0.
    + forwards~: chk_inf H0. eauto.
    + forwards~: chk_inf H. eauto.
    + forwards~: chk_inf H. forwards~: chk_inf H0. eauto.
  - inverts* IHHt1. inverts* IHHt2. 
    destruct* x; destruct* x0.
    + forwards~: chk_inf H0. eauto.
    + forwards~: chk_inf H. eauto.
    + forwards~: chk_inf H. forwards~: chk_inf H0. eauto.
  - inverts* IHHt. destruct* x.
  - inverts* IHHt. destruct* x.
    forwards~: chk_inf H0. eauto.
  - inverts* IHHt1. inverts* IHHt2. 
    destruct* x; destruct* x0.
    + forwards~: chk_inf H0. eauto.
    + forwards~: chk_inf H0. eauto.
  - inverts* IHHt. destruct* x. 
    forwards~: chk_inf H. eauto.
  - inverts* IHHt. destruct* x. 
    forwards~: chk_inf H0. eauto.
Qed.