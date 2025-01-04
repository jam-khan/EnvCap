# λ<sub>E</sub> Calculus and Extensions: Typing Rules and Big-Step Semantics

## Introduction
This document provides an overview of the Typing Rules and Big-Step Semantics in λ<sub>E</sub> Calculus. It serves as a reference for understanding the formal rules and evaluation strategies used.

## Big-Step Operational Semantics

     -----------
    | v ⊢ e ⇒ e'|                                                                           (Big-step Operational Semantics)
     -----------
                        ----------- BSTEP-CTX        ----------- BSTEP-LIT       ----------- BSTEP-UNIT  
                        v |- ? => v                  v |- i => i                 v |- ε => ε 

                            v |- e => v1
                    -------------------------- BSTEP-PROJ       ------------------------------- BSTEP-CLOS
                    v |- e.n => lookupv(v1, n)                  v |- <v1, λA. e> => <v1, λA. e> 

                        v |- e1 => <v1, λA. e>      v |- e2 => v2       v1 ,, v2 |- e => v'
                        ------------------------------------------------------------------- BSTEP-APP
                                                v |- e1 e2 => v

                                                    v |- e => v1                      v |- e => v1    v1.l ⇝ v2
        ------------------------ BSTEP-LAM      ------------------------ BSTEP-REC      ------------------------- BSTEP-SEL
        v |- λA. e => <v, λA. e>                v |- {l = e} => {l = v1}                      v |- e.l => v2  
        
            v |- e1 => v1       v ,, v1 |- e2 => v2                   v |- e1 => v1     v1 |- e2 => v2  
            --------------------------------------- BSTEP-MERGE       -------------------------------- BSTEP-BOX
                v |- e1 ,, e2 => v1 ,, v2                                 v |- e1 ▷ e2 => v2
            
                                            ------------------------- BSTEP-VAR
                                            v |- _n_ => lookupv(v, n)

                                            lookupv(v1 ,, v2, 0) = v2
                                        lookupv(v1,,v2, n + 1) = lookupv(v1, n)

Where `_n_` is the sugar for de-bruijn index 

## Typying Rules

     -------------                                                                      (Containment)
    | l : A ∈ B   |
     -------------
    
                                        l : A ∈ B       l ∉ label(C)            l : A ∈ C   l ∉ label(B)  
            --------------- CTM-RCD     ---------------------------- CTM-ANDL   ------------------------ CTM-ANDR 
            l : A ∈ {l : A}                    l : A  ∈  B & C                      l : A l ∈ B & C     


     -----------
    | Γ ⊢ e : A |                                                                       (Typing)
     -----------
                                        Γ ⊢ e : B       lookup(B, n) = A 
                --------- TYP-CTX       -------------------------------- TYP-PROJ       ------------ TYP-LIT
                Γ ⊢ ? : Γ                          Γ ⊢ e : A                             Γ ⊢ i : Int

                                Γ ⊢ e1 : Γ1      Γ1 ⊢ e2 : A                Γ ⊢ e1 : A       Γ & A ⊢ e2 : B
        ---------- TYP-TOP      ----------------------------- TYP-BOX       -------------------------------- TYP-MERGE
        v |- ε : ε                     Γ ⊢ e1 ▷ e2 : A                            Γ ⊢ e1 ,, e2 : A & B

                        Γ ⊢ e1 : A → B      Γ ⊢ e2 : A                    Γ & A ⊢ e : B
                        ------------------------------ TYP-APP          ------------------ TYP-LAM
                                Γ ⊢ e1 e2 : B                           Γ ⊢ λA. e : A → B
        
        ε ⊢ v : Γ1      Γ1 & A ⊢ e : B                    Γ ⊢ e : A                     Γ ⊢ e : B   l : A ∈ B
        ------------------------------- TYP-CLOS    --------------------- TYP-RCD       ----------------------  TYP-SEL
            Γ ⊢ ⟨v, λA. e⟩ : A → B                  Γ ⊢ {l = e} : {l : A}                    Γ ⊢ e.l : A     

                                        
                                            lookup(A & B, 0) = B
                                        lookup(A & B, n + 1) = lookup(A, n)

<!--    
### Variables
(T-Var) Γ(x) = T ------------ Γ ⊢ x : T


### Abstraction
(T-Abs) Γ, x:T1 ⊢ t2 : T2 ----------------- Γ ⊢ λx:T1.t2 : T1→T2


### Application

  Γ ⊢ t1 : T1→T2       Γ ⊢ t2 : T1
------------------------------------ (T-App) 
            Γ ⊢ t1 t2 : T2


### Let Binding
(T-Let) Γ ⊢ t1 : T1 Γ, x:T1 ⊢ t2 : T2 ------------------------------ Γ ⊢ let x = t1 in t2 : T2


### Conditional
(T-If) Γ ⊢ t1 : Bool Γ ⊢ t2 : T Γ ⊢ t3 : T -------------------------------------- Γ ⊢ if t1 then t2 else t3 : T


## Big-Step Semantics

### Evaluation of Variables
(E-Var) ------------- x ⇓ v (x is a variable)


### Evaluation of Abstraction
(E-Abs) ------------- λx.t ⇓ λx.t


### Evaluation of Application
(E-App) t1 ⇓ λx.t12 t2 ⇓ v2 t12[x:=v2] ⇓ v ------------------------------------- t1 t2 ⇓ v


### Evaluation of Let Binding
(E-Let) t1 ⇓ v1 t2[x:=v1] ⇓ v2 ------------------------ let x = t1 in t2 ⇓ v2


### Evaluation of Conditional
(E-If-True) t1 ⇓ true t2 ⇓ v2 ------------------- if t1 then t2 else t3 ⇓ v2

(E-If-False) t1 ⇓ false t3 ⇓ v3 ------------------- if t1 then t2 else t3 ⇓ v3


## Conclusion
This README provides a concise overview of the Typing Rules and Big-Step Semantics in Lambda Calculus. These formal rules and evaluation strategies form the foundation of understanding and reasoning about Lambda Calculus expressions. -->
