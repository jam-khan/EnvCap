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

Inductive op := app | mrg.

Inductive exp :=
  | ctx         : exp
  | lit         : nat -> exp
  | unit        : exp
  | binop       : op -> exp -> exp -> exp
  | proj        : exp -> nat -> exp
  | clos        : exp -> typ -> exp -> exp
  | rec         : string -> exp -> exp
  | rproj       : exp -> string -> exp.

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
      has_type E e1 (arr A B) ->
      has_type E e2 A ->
      has_type E (binop app e1 e2) B
  | tmrg : forall E e1 e2 A B,
      has_type E e1 A -> 
      has_type (and E A) e2 B ->
      has_type E (binop mrg e1 e2) (and A B)
  | tproj: forall E A e B n,
      has_type E e A ->
      lookup A n B ->
      has_type E (proj e n) B
  | tclos : forall E E1 A e ev B,
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
  | sbmrg : forall v v1 e' e,
      value v ->
      value v1 ->
      step (binop mrg v v1) e e' -> 
      step v (binop mrg v1 e) (binop mrg v1 e')
  | sbeta : forall v v1 v2 A e e', 
      value v   -> 
      value v1  -> 
      value v2 ->
      step (binop mrg v2 v1) e e' ->
      step v (binop app (clos v2 A e) v1) (binop app (clos (binop mrg v2 v1) top e') unit)
  | sbetav : forall v v1 v2,
      value v -> 
      value v1 -> 
      value v2 -> 
      step v (binop app (clos v1 top v2) unit) v2
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
Hint Constructors typ op exp lookup lookupv lin rlookup has_type value rlookupv step : core.


Require Import Program.Equality.


(* Type soundness *)
Lemma value_weaken : forall E v A,
  has_type E v A -> forall E',
  value v ->
  has_type E' v A.
Proof. 
  intros E v A H.
  induction H;
  intros E' Hv.
  + inversion Hv.
  + apply tint. 
  + apply tunit.
  + inversion Hv.
  + inversion Hv; subst.
    - apply tmrg.
      ++ apply IHhas_type1. assumption.
      ++ apply IHhas_type2. assumption.
  + inversion Hv.
  + inversion Hv; subst.
    apply tclos with (E1 := E1); assumption.
  + inversion Hv; subst. apply trcd. apply IHhas_type. assumption.
  + inversion Hv.
Qed.


Lemma lookupv_value: forall v n v',
  lookupv v n v' ->
  value v        ->
  value v'.
Proof.
  introv Hl.
  induction Hl; intros Hv.
  + inversion Hv; assumption.
  + inversion Hv; subst. apply IHHl. assumption.
Qed.

Lemma lookup_pres_ctx: forall E n A,
  lookup E n A -> forall v v',
  lookupv v n v' -> 
  value v ->
  has_type top v E ->
  has_type E v' A.
Proof.
  introv Hl. inductions Hl; introv Hlv Hv Ht.
  - inverts* Hlv. inverts* Ht. inverts Hv.
    eapply value_weaken; eauto.
  - inverts* Hlv. inverts* Ht. inverts Hv.
    forwards~: IHHl H1 H2.
    eapply value_weaken; eauto.
    eapply lookupv_value; eauto.
Qed.

Lemma lookup_pres: forall E n A,
  lookup E n A -> forall v v',
  lookupv v n v' -> 
  value v ->
  has_type top v E ->
  has_type top v' A.
Proof.
  introv Hl Hlv Hv Ht.
  forwards~: lookup_pres_ctx Hl Hlv Hv Ht.
  eapply value_weaken; eauto.
  eapply lookupv_value; eauto. 
Qed.


Lemma rlookupv_value: forall v l v', 
  rlookupv v l v' -> 
  value v -> 
  value v'.
Proof.
  introv Hl. inductions Hl; introv Hv; inverts* Hv.
Qed.


Lemma rcd_shape: forall E v l B, 
  has_type E v (rcd l B) -> 
  value v -> exists v',
  v = rec l v'.
Proof.
  introv Ht. inductions Ht; introv Hv;
  try solve [inverts* Hv].
Qed. 

Lemma notin_false: forall E v A,
  has_type E v A -> forall l v',
  rlookupv v l v' ->
  ~ lin l A ->
  False.
Proof.
  introv Ht. inductions Ht; introv Hr Hnl;
  try solve [inverts* Hr].
Qed.

Lemma rlookup_pres_aux: forall E l A,
  rlookup E l A -> forall v v',
  rlookupv v l v' -> 
  value v ->
  has_type top v E ->
  has_type E v' A.
Proof.
  introv Hl. inductions Hl; introv Hlv Hv Ht.
  - inverts* Ht; try solve [inverts Hv].
    inverts Hlv. inverts Hv.
    eapply value_weaken; eauto.
  - inverts* Hlv; try solve [inverts* Ht]. 
    + inverts Ht. inverts Hv.
      forwards~: IHHl H0 H5. 
      eapply value_weaken; eauto.
      eapply rlookupv_value; eauto.
    + inverts Ht. inverts Hv. 
      exfalso. eapply notin_false; eauto.
  - inverts* Hlv; try solve [inverts* Ht]. 
    + inverts Ht. inverts Hv. 
      exfalso. eapply notin_false; try eapply H; eauto.
    + inverts Ht. inverts Hv.
      forwards~: IHHl H0. 
      eapply value_weaken; eauto.
      eapply value_weaken; eauto.
      eapply rlookupv_value; eauto.
Qed. 

Lemma rlookup_pres: forall E l A,
  rlookup E l A -> forall v v',
  rlookupv v l v' -> 
  value v ->
  has_type top v E ->
  has_type top v' A.
Proof.
  introv Hl Hlv Hv Ht.
  forwards~: rlookup_pres_aux Hl Hlv Hv Ht.
  eapply value_weaken; eauto.
  eapply rlookupv_value; eauto.
Qed.

(* 
  7 goals
  v : exp
  H : value v
  E, A : typ
  Ht : has_type E ctx A
  Htv : has_type top v E
  ______________________________________(1/7)
  has_type E v A
  ______________________________________(2/7)
  has_type E (binop box v1 e2') A
  ______________________________________(3/7)
  has_type E (binop mrg v1 e') A
  ______________________________________(4/7)
  has_type E (binop box (binop mrg v2 v1) e) A0
  ______________________________________(5/7)
  has_type E v2 A
  ______________________________________(6/7)
  has_type E v2 A
  ______________________________________(7/7)
  has_type E v2 A

*)
Definition useful : forall A e B,
    has_type A e B ->
    has_type (and A top) e B.
Proof.
Admitted.

Definition useful2 : forall A e B,
  has_type A e B ->
  has_type A e (and B top).
Admitted.

Lemma gpreservation : forall e e' v, 
  step v e e' -> forall E A, 
  has_type E e A -> 
  has_type top v E -> 
  has_type E e' A.
Proof.
  introv Hs. inductions Hs; introv Ht Htv;
  try solve [eauto];
  try solve [inverts* Ht].
  - inversion Ht; subst.
    apply value_weaken with (E := top); assumption.
  - inverts* Ht.
    econstructor; eauto.
    eapply IHHs; eauto.
    econstructor; eauto.
    eapply value_weaken; eauto.
  - inversion Ht; subst.
    inversion H5; subst.
    apply tapp with (A := top).
    + apply tclos with (E1 := and E1 A1).



inversion Ht; subst.
    apply tapp with (A
    + 
    apply tclos with (E1 := and A1 A1).
      -- apply tmrg; eauto using value_weaken.
      ++ apply IHHs.

  - inverts* Ht.
    forwards~: lookup_pres H7 H1.
    eapply value_weaken; eauto.
    eapply value_weaken; eauto.
    eapply lookupv_value; eauto.
  - inverts* Ht.
    forwards~: rlookup_pres H7 H1.
    eapply value_weaken; eauto.
    eapply value_weaken; eauto.
    eapply rlookupv_value; eauto.
Qed.

Lemma lookup_prog: forall n A B,
  lookup A n B -> forall v,
  has_type top v A ->
  value v ->
  exists e', lookupv v n e'.
Proof.
  introv Hl. inductions Hl; introv Ht Hv.
  - inverts* Ht; try solve [inverts* Hv].
  - inverts* Ht; try solve [inverts* Hv].
    + inverts* Hv. 
      forwards~ (e'&Hs): IHHl H3.
      inverts* Hs.
Qed.

Lemma rlookup_prog: forall n A B,
  rlookup A n B -> forall v,
  has_type top v A ->
  value v ->
  exists e', rlookupv v n e'.
Proof.
  introv Hl. inductions Hl; introv Ht Hv.
  - inverts* Ht; try solve [inverts* Hv].
  - inverts* Ht; try solve [inverts* Hv].
    inverts Hv.
    forwards~ (?&?): IHHl H4.
    exists* x.
  - inverts* Ht; try solve [inverts* Hv].
    inverts Hv.
    forwards~ (?&?): IHHl e2.
    eapply value_weaken; eauto.
    exists* x.
Qed.


Lemma gprogress : forall E e A, 
  has_type E e A -> forall v, 
  value v -> 
  has_type top v E -> 
  value e \/ exists e', step v e e'.
Proof.
  introv Ht.
  inductions Ht; introv Hv Htv; eauto.
  - right.
    forwards~: IHHt1 Htv.
    forwards~: IHHt2 Htv.
    destruct* H; destruct* H0.
    + inverts* Ht1; inverts* H.
    + inverts* H0.
    + inverts* H.
    + inverts* H.
  - forwards~: IHHt1 Htv.
    destruct H.
    + forwards~: IHHt2 H.
      eapply value_weaken; eauto.
      destruct* H0. right*. inverts* H0.
    + inverts* H.
  - forwards~: IHHt1 Htv.
    destruct H.
    + forwards~: IHHt2 (binop mrg v e1).
      econstructor; eauto.
      eapply value_weaken; eauto.
      destruct* H0.
      destruct* H0.
    + inverts* H.
  - right.
    forwards~: IHHt Hv.
    inverts H0.
    + forwards~: value_weaken Ht top.
      forwards~ (?&?): lookup_prog H H0.
      exists* x.
    + inverts* H1.
  - forwards~: IHHt Hv Htv. destruct* H.
    right*. destruct* H. 
  - right.
    forwards~: IHHt Hv.
    inverts H0.
    + forwards~: value_weaken Ht top.
      forwards~ (?&?): rlookup_prog H H0.
      exists* x. 
    + inverts* H1.
Qed.

Lemma preservation : forall e e' A, has_type top e A -> step unit e e' -> has_type top e' A.
  intros.
  eapply gpreservation; eauto.
Defined.
      
Lemma progress :  forall e A, has_type top e A -> value e \/ exists e', step unit e e'.
  intros.
  eapply gprogress; eauto.
Defined.




















