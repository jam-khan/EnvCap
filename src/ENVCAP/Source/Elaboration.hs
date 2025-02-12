{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ENVCAP.Source.Elaboration where
import ENVCAP.Syntax
import ENVCAP.Source.Errors 

elaborateTyp :: SourceTyp -> CoreTyp
elaborateTyp TySUnit               = TyCUnit
elaborateTyp TySInt                = TyCInt
elaborateTyp TySBool               = TyCBool
elaborateTyp TySString             = TyCString
elaborateTyp (TySAnd ty1 ty2)      = TyCAnd (elaborateTyp ty1) (elaborateTyp ty2)
elaborateTyp (TySArrow ty1 ty2)    = TyCArrow (elaborateTyp ty1) (elaborateTyp ty2)
elaborateTyp (TySRecord label ty)  = TyCRecord label (elaborateTyp ty)
elaborateTyp (TySList ty)          = TyCList (elaborateTyp ty)
elaborateTyp (TySSum ty1 ty2)      = TyCSum  (elaborateTyp ty1) (elaborateTyp ty2)
elaborateTyp (TySPair ty1 ty2)     = TyCPair (elaborateTyp ty1) (elaborateTyp ty2)
elaborateTyp (TySSig tA tB)        = TyCArrow (elaborateTyp tA) (elaborateTyp tB)

unescape :: String -> String
unescape [] = []
unescape ('\\' : 'n' : xs) = '\n' : unescape xs  -- Replace `\n` with newline
unescape ('\\' : '\\' : xs) = '\\' : unescape xs -- Replace `\\` with `\`
unescape ('\\' : '\"' : xs) = '\"' : unescape xs -- Replace `\"` with `"`
unescape (x : xs) = x : unescape xs

type Elab = (SourceTyp, CoreTm)

type Context    = SourceTyp
type Message    = String
type Suggestion = String

generateError :: Context -> SourceTm -> Message -> Suggestion -> SourceTypeError
generateError ctx tm msg sugg =
        STypeError
                ("Typechecking failed at Source level\n\n" ++
                "Context: " ++ show ctx ++ "\n" ++
                "Expression: " ++ show tm ++ "\n" ++
                msg     ++ "\n" ++
                sugg    ++ "\n")

-- Lookup based on indexing
lookupt :: SourceTyp -> Int -> Maybe SourceTyp
lookupt (TySAnd _ tB) 0          = Just tB
lookupt (TySAnd tA _) n          = lookupt tA (n - 1)
lookupt _ _                       = Nothing

-- checks if l is a label in the typing context
isLabel :: String -> SourceTyp -> Bool
isLabel l (TySRecord label _)     = l == label
isLabel l (TySAnd tA tB)          = isLabel l tA || isLabel l tB
isLabel _ _                       = False

-- containment
containment :: SourceTyp -> SourceTyp -> Bool
containment (TySRecord l tA) (TySRecord label typ ) 
                                = l == label && tA == typ
containment (TySRecord l tA) (TySAnd tB tC) 
                                =   (containment (TySRecord l tA) tB && not (isLabel l tC)) ||
                                    (containment (TySRecord l tA) tC && not (isLabel l tB))
containment _ _                 = False

-- Lookup based on label
rlookupt :: SourceTyp -> String -> Maybe SourceTyp
rlookupt (TySRecord l t) label
    | l == label = Just t
rlookupt (TySAnd tA tB) label 
                        = case rlookupt tB label of
                                Just t    -> Just t
                                Nothing   -> rlookupt tA label
rlookupt _ _            = Nothing

elaborateInfer :: SourceTyp -> SourceTm -> Either SourceTypeError Elab
elaborateInfer ctx TmCtx             = Right (ctx, Ctx)
elaborateInfer _ TmUnit            = Right (TySUnit, Unit)
elaborateInfer _ (TmLit i)         = Right (TySInt, Lit i)
elaborateInfer _ (TmBool b)        = Right (TySBool, EBool b)
elaborateInfer _ (TmString s)      = Right (TySString, EString s)
elaborateInfer ctx (TmLam tA tm)     = 
                case elaborateInfer (TySAnd ctx tA) tm of
                        Right (tB, tm') -> Right (TySArrow tA tB, Lam (elaborateTyp tA) tm')
                        Left (STypeError err) -> 
                                Left $ generateError ctx tm
                                        "Couldn't infer the type of the term inside abstraction."
                                        ("Check the term inside abstraction. \n \n-----Further info-----\n \n" ++ err)
elaborateInfer ctx (TmClos e1 tA e2) = 
                case elaborateInfer TySUnit e1 of
                        Right (gamma', e1') ->
                                case elaborateInfer (TySAnd gamma' tA) e2 of
                                        Right (tB, e2') -> Right 
                                                (TySArrow tA tB, Box e1' (Lam (elaborateTyp tA) e2'))
                                        Left err        -> Left err
                        Left (STypeError err) -> 
                                Left $ generateError ctx (TmClos e1 tA e2)
                                        "Couldn't infer the type of closure environment."
                                        ("Check the environment passed into the closure. \n \n \n-----Further info-----\n \n \n" ++ err)

elaborateInfer ctx (TmRec l tm)     = 
                case elaborateInfer ctx tm of
                        Right (tA, tm')         -> 
                                Right (TySRecord l tA, Rec l tm')
                        Left  (STypeError err)  -> 
                                Left $ generateError ctx (TmRec l tm) "Typecheck failed at a record/assignment." err
elaborateInfer ctx (TmRProj tm l)   = 
                case elaborateInfer ctx tm of
                        Right (tB, tm') -> 
                                case rlookupt tB l of
                                        Just tA -> 
                                                if containment (TySRecord l tA) tB
                                                        then Right (tA, RProj tm' l)
                                                        else Left $ generateError ctx tm 
                                                                "Containment failed for labeled lookup." 
                                                                "Make sure that are no ambiguous lookups."
                                        Nothing -> Left $ generateError ctx tm 
                                                                "Lookup failed. Label doesn't exists!" 
                                                                ("Check whether label " ++ show l ++ " exists in the record.")
                        Left err        -> Left err
elaborateInfer ctx (TmProj tm n)    = 
                case elaborateInfer ctx tm of
                        Right (tB, tm')         -> case lookupt tB n of
                                                        Just tA  -> Right (tA, Proj tm' n)
                                                        Nothing  -> Left $ generateError tB tm 
                                                                        "Type error on index lookup failed." 
                                                                        "Make sure the term is present in context."
                        Left (STypeError err)   -> Left $ generateError ctx tm 
                                                                "Type error on term for projection."
                                                                ("Make sure that term is correct before projecting\n \n-----Further info-----\n \n" ++ err)
elaborateInfer ctx (TmApp tm1 tm2)  
                = case elaborateInfer ctx tm1 of
                        Right (typ, tm1')       
                                -> case typ of
                                        (TySArrow tA tB)   
                                                -> case elaborateCheck ctx tm2 tA of
                                                        Right tm2'      -> Right (tB, App tm1' tm2')
                                                        Left (STypeError err)
                                                                        -> Left $ generateError ctx tm2
                                                                                        ("Type error on application: expected type " ++ show tA)
                                                                                        err
                                        _       -> Left $ 
                                                        generateError ctx tm1
                                                        ("Type error on application: function type expected, but got" ++ show typ)
                                                        "Make sure that function is well-defined."
                        Left (STypeError err) -> 
                                Left $ generateError ctx tm1 
                                        "Type error on application"
                                        ("Check the function \n \n-----Further info-----\n \n" ++ err)
elaborateInfer ctx (TmMrg tm1 tm2)  
                = case elaborateInfer ctx tm1 of
                        Right (ty1, tm1')      -> 
                                case elaborateInfer (TySAnd ctx ty1) tm2 of
                                        Right (ty2, tm2')       -> Right (TySAnd ty1 ty2, Mrg tm1' tm2')
                                        Left  (STypeError err)  -> Left $ generateError (TySAnd ctx ty1) tm2
                                                                                        "Type error on second term in merge." 
                                                                                        ("Check if second part of merge is well-defined. \n \n-----Further info-----\n \n" ++ err)
                        Left (STypeError err)   ->
                                Left $  generateError ctx (TmMrg tm1 tm2) 
                                        "Type error on first-part of merge."
                                        ("Check if first part of merge is well-defined. \n \n-----Further info-----\n \n" ++ err)
elaborateInfer ctx (TmBox tm1 tm2)  
                = case elaborateInfer ctx tm1 of
                        Right (ctx', tm1') -> 
                                case elaborateInfer ctx' tm2 of
                                        Right (ty, tm2')        -> 
                                                Right (ty, Box tm1' tm2')
                                        Left (STypeError err)   -> 
                                                Left $ generateError ctx' tm2 
                                                        "Type error on the term inside the box."
                                                        ("Check if the term inside the box is well-defined.\n \n-----Further info-----\n \n" ++ err)
                        Left (STypeError err) -> 
                                Left $ generateError ctx tm1 
                                        "Type error on box construct"
                                        ("Make sure that the environment for box construct is correct.\n \n-----Further info-----\n \n" ++ err)
elaborateInfer ctx (TmIf tm1 tm2 tm3) 
                = case elaborateCheck ctx tm1 TySBool of
                        Right tm1'      ->
                                case elaborateInfer ctx tm2 of
                                        Right (ty2, tm2') -> 
                                                case elaborateCheck ctx tm3 ty2 of
                                                        Right tm3'      -> 
                                                                Right (ty2, If tm1' tm2' tm3')
                                                        Left (STypeError _err) -> 
                                                                Left $ generateError ctx tm3
                                                                        "Type error on the if statement: types don't match of then and else branch."
                                                                        "Make sure that the types of both branches are same"
                                        Left (STypeError err) ->
                                                Left $ generateError ctx tm2
                                                        "Type error on the then branch of if statement"
                                                        ("Make sure that the term inside `then` is well-defined and sound.\n----Further info-----\n" ++ err)
                        Left (STypeError err)   ->
                                Left $ generateError ctx tm1
                                                "Type error on condition: condition must be of type Bool"
                                                ("Fix the condition and make sure it is of type Bool.\n \n-----Further info-----\n \n" ++ err)   
elaborateInfer ctx (TmFix (TmLam tA tm)) = 
                case elaborateInfer (TySAnd ctx (TySArrow tA tA)) (TmLam tA tm) of
                        -- IMPORTANT
                        -- This is not the right way to handle it (Add annotations on surface level)
                        -- Temporary fix for testing purposes
                        Right (ty, tm')       -> case elaborateTyp (TySArrow ty ty) of
                                                        (TyCArrow t1 _) -> Right (ty, Fix t1 tm')
                                                        _               -> Left $ generateError ctx (TmFix (TmLam tA tm)) 
                                                                                "Type error on fixpoint"
                                                                                "Make sure fixpoint error is correct"
                        Left (STypeError err)   
                                        -> Left $ generateError ctx (TmFix (TmLam tA tm)) 
                                                "Type error on fixpoint"
                                                err
elaborateInfer ctx (TmBinOp (Arith op) tm1 tm2) =
                case elaborateCheck ctx tm1 TySInt of
                        Right tm1' -> 
                                case elaborateCheck ctx tm2 TySInt of
                                        Right tm2'      -> 
                                                Right (TySInt, BinOp (Arith op) tm1' tm2')
                                        Left (STypeError err) ->
                                                Left $ generateError ctx (TmBinOp (Arith op) tm1 tm2)
                                                        ("Type error on arithmetic operation `" ++ show op ++ "`. Types on both sides must be Int.")
                                                        ("Make sure that types of both operands are Int.\n \n-----Further info-----\n \n" ++ err)
                        Left err ->
                                Left $ generateError ctx tm1 
                                        ("Type error on first operand of the operator" ++ show op)
                                        ("First operand has type error.\n \n-----Further info-----\n \n" ++ show err)
elaborateInfer ctx (TmBinOp (Comp op) tm1 tm2) =
                case elaborateInfer ctx tm1 of
                        Right (TySInt, tm1') -> 
                                case elaborateCheck ctx tm2 TySInt of
                                        Right tm2'      -> 
                                                Right (TySBool, BinOp (Comp op) tm1' tm2')
                                        Left (STypeError err) ->
                                                Left $ generateError ctx (TmBinOp (Comp op) tm1 tm2)
                                                        ("Type error on comparsion operation `" ++ show op ++ "`. Types on both sides must be Int.")
                                                        ("Make sure that types of both operands are Int.\n \n-----Further info-----\n \n" ++ err ++ "\n")
                        Right (TySBool, tm1') ->
                                case elaborateCheck ctx tm2 TySBool of
                                        Right tm2'      -> 
                                                Right (TySBool, BinOp (Comp op) tm1' tm2')
                                        Left (STypeError err) ->
                                                Left $ generateError ctx (TmBinOp (Comp op) tm1 tm2)
                                                        ("Type error on comparsion operation `" ++ show op ++ "`. Types on both sides must be Bool.")
                                                        ("Make sure that types of both operands are Bool.\n \n-----Further info-----\n \n" ++ err)
                        Right (TySString, tm1') ->
                                case elaborateCheck ctx tm2 TySString of
                                        Right tm2'      -> 
                                                Right (TySBool, BinOp (Comp op) tm1' tm2')
                                        Left (STypeError err) ->
                                                Left $ generateError ctx (TmBinOp (Comp op) tm1 tm2)
                                                        ("Type error on comparsion operation `" ++ show op ++ "`. Types on both sides must be String.")
                                                        ("Make sure that types of both operands are String.\n \n-----Further info-----\n \n" ++ err)
                        Right (_, _) ->
                                        Left $ generateError ctx (TmBinOp (Comp op) tm1 tm2)
                                                        ("Type error on comparsion operation `" ++ show op ++ "`. Types on both sides must be String.")
                                                        "Make sure that types of both operands are of same type.\n \n-----Further info-----\n \n"
                        Left (STypeError err) ->
                                Left $ generateError ctx tm1 
                                        ("Type error on first operand of the comparison operator" ++ show op)
                                        ("First operand has type error.\n \n-----Further info-----\n \n" ++ err)
elaborateInfer ctx (TmBinOp (Logic op) tm1 tm2) =
                case elaborateCheck ctx tm1 TySBool of
                        Right tm1' ->
                                case elaborateCheck ctx tm2 TySBool of
                                        Right tm2'      -> 
                                                Right (TySBool, BinOp (Logic op) tm1' tm2')
                                        Left (STypeError err) ->
                                                Left $ generateError ctx (TmBinOp (Logic op) tm1 tm2)
                                                        ("Type error on logic operation `" ++ show op ++ "`. Types on both sides must be Bool.")
                                                        ("Make sure that types of both operands are Bool.\n \n-----Further info-----\n \n" ++ err)
                        Left (STypeError err) ->
                                Left $ generateError ctx tm1 
                                        ("Type error on first operand of the logic operator" ++ show op)
                                        ("First operand has type error.\n \n-----Further info-----\n \n" ++ err)
elaborateInfer ctx (TmUnOp Not tm)     =
                case elaborateInfer ctx tm of
                        Right (ty, tm')         -> Right (ty, UnOp Not tm')
                        Left err                -> Left err


elaborateCheck :: SourceTyp -> SourceTm -> SourceTyp -> Either SourceTypeError CoreTm
elaborateCheck ctx tm typ       
        = case elaborateInfer ctx tm of
                Right (typ', e') ->
                        if typ == typ'
                                then Right e'
                                else Left  $ generateError ctx tm
                                                ("Couldn't match expected type \'" ++ show typ ++ "\' with actual type \'" ++ show typ' ++ "\'")
                                                "Please check your code."
                Left err        -> Left err
