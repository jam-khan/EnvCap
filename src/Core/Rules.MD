# λ<sub>E</sub> Calculus and Extensions: Typing Rules and Big-Step Semantics

## Introduction
This document provides an overview of the Typing Rules and Big-Step Semantics in λ<sub>E</sub> Calculus. It serves as a reference for understanding the formal rules and evaluation strategies used.

## Syntax

    Types               A, B, Γ ::= Int | ε | A & B | A → B | {l : A}
    Expressions               e ::= ? | e.n | i | ε | λA. e | ⟨v, λA. e⟩ | e1 e2 | e1 ,, e2 | {l = e} | e.l
    Values                    v ::= i | ε | ⟨v, λA. e⟩ | v1 ,, v2 | {l = v}
    Frames                    F := [ ].n | [ ] # e | [ ] e | [ ] ▷ e | v [ ] | {l = [ ]} | [ ].l
    Syntactic Sugar         _n_ ≜ ?.n
                            _l_ ≜ ?.l


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


## Extensions
The λ<sub>E</sub> has been extended with arithmetic, logic, comparison operations. Furthermore, conditionals, recursion, sums, pairs, lists, boolean, and strings have been added.


## Syntax

    Types                     T ::= ... | Bool | String | List T | T1 + T2 | (T1, T2)
    Expressions               e ::= ... | b | s | let e1 e2 | if e1 e2 e3 | Fix e 
                                        | (e1, e2) | fst e | snd e 
                                        | inl T e | inr T e | case e1 e2 e3 
                                        | nil T | cons e1 e2 | lcase e1 e2 e3 
                                        | unaryop e 
                                        | e1 binop e2
    
    Values                    v ::= ... | True | False | s | (v1, v2) | inl T v | inr T v | nil T | cons v v  
    
    Binary operations     binop ::= arithop | compop | logicop
                        arithop ::= + | - | * | / | %
                         compop ::= < | <= | == | !== | >= | >
                        logicop ::= & | |
    
    Unary operations    unaryop ::= !

## Big-Step Operational Semantics: λ<sub>E</sub> extensions


            -------------- BSTEP-True               --------------- BSTEP-False                 ----------- BSTEP-STR          
            v |- b => True                          v |- b => False                             v |- s => s                   

                    v |- e => ⟨v1, λA e1⟩                               v |- e1 => v1      v ,, v1 |- e2 => v1
                ------------------------------- BSTEP-FIX               -------------------------------------- BSTEP-LET
                v |- Fix e  => ⟨v1, Fix λA. e1⟩                                  v |- let e1 e2 => v1

                v |- e1 => True     v |- e2 => v2                         v |- e1 => False     v |- e3 => v3
                --------------------------------- BSTEP-IF_True           ---------------------------------- BSTEP-IF_False
                    v |- if e1 e2 e3 => v2                                      v |- if e1 e2 e3 => v3
                    
            v |- e1 => v1   v |- e2 => v2                   v |- e => (v1, v2)                  v |- e => (v1, v2)
            ----------------------------- BSTEP-PAIR        ------------------ BSTEP-Fst        ------------------ BSTEP-Snd
                v |- (e1, e2) => (v1, v2)                   v |- Fst e => v1                    v |- Snd e => v2

                                v |- e => v1                                    v |- e => v1
                            ------------------------ BSTEP-Inl              ---------------------- BSTEP-Inr
                            v |- inl T e => inl T v1                        v |- inr T => inl T v1

            v |- e1 => inl T v1     v ,, v1 |- e2 => v2                      v |- e1 => inl T v1     v ,, v1 |- e3 => v3
            ------------------------------------------- BSTEP-CASEInl        ------------------------------------------- BSTEP-CASEInr
                        v |- case e1 e2 e3 => v3                                          v |- case e1 e2 e3 => v3

                                                                        v |- e1 => v1   v |- e2 => v2
                            ------------------- BSTEP-Nil               ----------------------------- BSTEP-Cons
                            v |- nil T => nil T                         v |- cons e1 e2 => cons v1 v2
                
            v |- e1 => nil T    v |- e2 => v2                           v |- e1 => vcons v1 v2     v ,, v1 ,, v2 |- e3 => v3
            --------------------------------- BSTEP-LCASE_Nil           ---------------------------------------------------- BSTEP-LCASE_Cons
                v |- lcase e1 e2 e3 => v2                                             v |- lcase e1 e2 e3 => v3              


            v |- e => v1    v |- unaryop v1 => v'                       v |- e1 => v1       v |- e2 => v2   v |- v1 binop v2 => v'                          
            ------------------------------------- BSTEP-Unaryop         --------------------------------------------------------- BSTEP-BinaryOp           
            v |- unaryop e => v'                                                            v |- e1 binop e2 v'


Arithmetic, Binary and Logical operations are the standard manipulations.


## Typing Rules:  λ<sub>E</sub> extensions


                                                                                    Γ |- e : T1 -> T1    
                        ------------ TYP-BOOL       --------------- TYP-STRING      ----------------- TYP-FIX    
                        Γ |- b : Bool               Γ |- s : String                  Γ |- Fix e : T1

        Γ |- e1 : A      Γ &  A |- e2 : B                       Γ |- e1 : Bool      Γ |- e2 : A     Γ |- e2 : B     A = B
        --------------------------------- TYP-LET               --------------------------------------------------------- TYP-IF
                Γ |- let e1 e2 : B                                              Γ |- if e1 e2 e3 : B
        
                Γ |- e1 : A     Γ |- e2 : B                     Γ |- e : A * B                  Γ |- e : A * B
                ---------------------------- TYP-PAIR           ---------------- TYP-Fst        -------------- TYP-Snd
                    Γ |- (e1, e2) : A * B                       Γ |- Fst e : A                  Γ |- Snd e : N

                                Γ |- e1 : T1                                  Γ |- e2 : T2
                        ------------------------ TYP-InL                ------------------------ TYP-InR
                        Γ |- inl T2 e1 : T1 + T2                        Γ |- inr T1 e2 : T1 + T2

                                Γ |- e0 : T1 + T2   Γ & T1 |- e1 : T3   Γ & T2 |- e2 : T3
                                --------------------------------------------------------- TYP-CASE
                                                Γ |- case e0 e1 e2 : T3

                            Γ |- e0 : List T1       Γ |- e1 : T2    (Γ & T1) & List T1 |- e2 : T2
                            --------------------------------------------------------------------- TYP-LCASE
                                                    Γ |- lcase e0 e1 e2 : T3
                            
                                                                    Γ |- e2 : List T    Γ |- e1 : T
                        ------------------- TYP-Nil                 ------------------------------- TYP-Cons
                        Γ |- nil T : List T                             Γ |- cons e1 e2 : List T
            

            Γ |- e1 : T1   Γ |- e2 : T1   Γ |- v1 binop v2 : T1                              Γ |- e : T
            ------------------------------------------------------ BSTEP-BinaryOp         -------------------- BSTEP-Unaryop
                             Γ |- e1 binop e2 : T1                                        Γ |- unaryop e : T

