markdown
# Lambda Calculus: Typing Rules and Big-Step Semantics

## Introduction
This document provides an overview of the Typing Rules and Big-Step Semantics in λ<sub>E</sub> Calculus. It serves as a reference for understanding the formal rules and evaluation strategies used.

## Big-Step Operational Semantics

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

## Typying Rules
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
