module ENVCAP.Source.Elaboration where
import ENVCAP.Core.Syntax as Core
import ENVCAP.Source.Syntax as Source


surfaceUnaryToCoreOp :: TmUnaryOp -> UnaryOp
surfaceUnaryToCoreOp TmNot              = Not

elaborateBinaryOp :: TmBinOp -> BinaryOp
elaborateBinaryOp (TmArith arithop)
        = case arithop of
                TmAdd   -> Arith Add
                TmSub   -> Arith Sub
                TmMul   -> Arith Mul
                TmDiv   -> Arith Div
                TmMod   -> Arith Mod
elaborateBinaryOp (TmComp compop)
        = case compop of
                TmEql   -> Comp Eql
                TmNeq   -> Comp Neq
                TmLt    -> Comp Lt
                TmLe    -> Comp Le
                TmGt    -> Comp Gt
                TmGe    -> Comp Ge
elaborateBinaryOp (TmLogic logicop)
        = case logicop of
                TmAnd   -> Logic And
                TmOr    -> Logic Or



elaborateTyp :: Source.Typ -> Maybe Core.Typ
elaborateTyp Source.TUnit               = Just Core.TUnit
elaborateTyp Source.TInt                = Just Core.TInt
elaborateTyp Source.TBool               = Just Core.TBool
elaborateTyp Source.TString             = Just Core.TString
elaborateTyp (Source.TAnd ty1 ty2)      = Core.TAnd             <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TArrow ty1 ty2)    = Core.TArrow           <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TRecord label ty)  = Core.TRecord label    <$> elaborateTyp ty
elaborateTyp (Source.TList ty)          = Core.TList            <$> elaborateTyp ty
elaborateTyp (Source.TSum ty1 ty2)      = Core.TSum             <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TPair ty1 ty2)     = Core.TPair            <$> elaborateTyp ty1 <*> elaborateTyp ty2
elaborateTyp (Source.TSig tA tB)        = Core.TArrow           <$> elaborateTyp tA  <*> elaborateTyp tB
elaborateTyp (Source.TIden _)           = Nothing


type Elab = (Source.Typ, Exp)
newtype SourceTypeError = STypeError String deriving Show

type Context    = Source.Typ
type Message    = String
type Suggestion = String

generateError :: Context -> Tm -> Message -> Suggestion -> SourceTypeError
generateError ctx tm msg sugg =
        STypeError
                ("Typechecking failed at Source level\n" ++
                "Context: " ++ show ctx ++ "\n" ++
                "Expression: " ++ show tm ++ "\n" ++
                msg     ++ "\n" ++
                sugg    ++ "\n")

-- Lookup based on indexing
lookupt :: Source.Typ -> Int -> Maybe Source.Typ
lookupt (Source.TAnd tA tB) 0          = Just tB
lookupt (Source.TAnd tA tB) n          = lookupt tA (n - 1)
lookupt _ _                            = Nothing

-- checks if l is a label in the typing context
isLabel :: String -> Source.Typ -> Bool
isLabel l (Source.TRecord label _)     = l == label
isLabel l (Source.TAnd tA tB)          = isLabel l tA || isLabel l tB
isLabel _ _                             = False

-- containment
containment :: Source.Typ -> Source.Typ -> Bool
containment (Source.TRecord l tA) (Source.TRecord label typ ) 
                                = l == label && tA == typ
containment (Source.TRecord l tA) (Source.TAnd tB tC) 
                                =   (containment (Source.TRecord l tA) tB && not (isLabel l tC)) ||
                                    (containment (Source.TRecord l tA) tC && not (isLabel l tB))
containment _ _                 = False

-- Lookup based on label
rlookupt :: Source.Typ -> String -> Maybe Source.Typ
rlookupt (Source.TRecord l t) label
    | l == label = Just t
rlookupt (Source.TAnd tA tB) label 
                        = case rlookupt tB label of
                                Just t    -> Just t
                                Nothing   -> rlookupt tA label
rlookupt _ _            = Nothing

{--
        |   TmFix       Tm
        |   TmBinOp     TmBinOp Tm Tm
        |   TmUnOp      TmUnaryOp Tm
        
        |   TmPair      Tm Tm
        |   TmFst       Tm
        |   TmSnd       Tm
        |   TmNil       Typ
        |   TmCons      Tm Tm
        |   TmCase      Tm
        |   TmInL       Tm
        |   TmInR       Tm
        -- Not sure if tagging is needed at source level -- can be simply added during elaboration to core
        |   TmAnno      Tm Typ                  -- Tm :: Typ
        |   TmTuple     [Tm]
        |   TmSwitch    Tm [(Tm, Tm)]
        |   TmVar       String
        |   TmStruct    Typ Tm
        |   TmFunc      String Typ Tm
        |   TmModule    String Typ Tm
        |   TmAliasTyp  String Typ
        |   TmLet       String Typ Tm Tm
        |   TmLetrec    String Typ Tm Tm
        deriving (Eq, Show)

-- Types
data Typ      =     TUnit                  -- Unit type for empty environment
                |   TInt                   -- Integer type
                -- Can be used for pair
                |   TAnd Typ Typ           -- Intersection type
                |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
                |   TRecord String Typ     -- Single-Field Record Type
                -- Extensions
                |   TBool                  -- Boolean type
                |   TString                -- String type
                |   TList  Typ             -- Type for built-in list 
                |   TSum   Typ Typ         -- Type for sums
                |   TPair  Typ Typ
                |   TSig   Typ Typ             -- Sig Type End
                |   TIden  String          -- Simply an alias
                deriving (Eq, Show)

--}

elaborateInfer :: Source.Typ -> Tm -> Either SourceTypeError Elab
elaborateInfer ctx TmCtx             = Right (ctx, Ctx)
elaborateInfer ctx TmUnit            = Right (Source.TUnit, Unit)
elaborateInfer ctx (TmLit i)         = Right (Source.TInt, Lit i)
elaborateInfer ctx (TmBool b)        = Right (Source.TBool, EBool b)
elaborateInfer ctx (TmString s)      = Right (Source.TString, EString s)
elaborateInfer ctx (TmLam tA tm)     = 
                case elaborateInfer (Source.TAnd ctx tA) tm of
                        Right (tB, tm') ->
                                case elaborateTyp tA of
                                        Just tA' -> Right (Source.TArrow tA tB, Lam tA' tm')
                                        _        -> 
                                                Left $  generateError ctx (TmLam tA tm)
                                                        ("Couldn't translate the type \'" ++ show tA ++ "\' to the core.")
                                                        "Please check the input type of the abstraction or function."
                        Left (STypeError err) -> 
                                Left $ generateError ctx tm
                                        "Couldn't infer the type of the term inside abstraction."
                                        ("Check the term inside abstraction. \n-----Further info-----\n" ++ err)
elaborateInfer ctx (TmClos e1 tA e2) = 
                case elaborateInfer Source.TUnit e1 of
                        Right (gamma', e1') ->
                                case elaborateInfer (Source.TAnd gamma' tA) e2 of
                                        Right (tB, e2') -> Right 
                                                (Source.TArrow tA tB, Box e1' (Lam tA' e2'))
                                                        where Just tA' = elaborateTyp tA
                                        Left err        -> Left err
                        Left (STypeError err) -> 
                                Left $ generateError ctx (TmClos e1 tA e2)
                                        "Couldn't infer the type of closure environment."
                                        ("Check the environment passed into the closure. \n-----Further info-----\n" ++ err)

elaborateInfer ctx (TmRec l tm)     = 
                case elaborateInfer ctx tm of
                        Right (tA, tm')         -> 
                                Right (tA, Rec l tm')
                        Left  (STypeError err)  -> 
                                Left $ generateError ctx (TmRec l tm) "Typecheck failed at a record/assignment." err
elaborateInfer ctx (TmRProj tm l)   = 
                case elaborateInfer ctx tm of
                        Right (tB, tm') -> 
                                case rlookupt tB l of
                                        Just tA -> 
                                                if containment (Source.TRecord l tA) tB
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
                        Left err                -> Left $ generateError ctx tm 
                                                                "Type error on term for projection."
                                                                ("Make sure that term is correct before projecting\n-----Further info-----\n" ++ show err)
elaborateInfer ctx (TmApp tm1 tm2)  
                = case elaborateInfer ctx tm1 of
                        Right (typ, tm1')       
                                -> case typ of
                                        (Source.TArrow tA tB)   
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
                                        ("Check the function \n-----Further info-----\n" ++ show err)
elaborateInfer ctx (TmMrg tm1 tm2)  
                = case elaborateInfer ctx tm1 of
                        Right (ty1, tm1')      -> 
                                case elaborateInfer (Source.TAnd ctx ty1) tm2 of
                                        Right (ty2, tm2')       -> Right (Source.TAnd ty1 ty2, Mrg tm1' tm2')
                                        Left  (STypeError err)  -> Left $ generateError (Source.TAnd ctx ty1) tm2
                                                                                        "Type error on second term in merge." 
                                                                                        ("Check if second part of merge is well-defined. \n-----Further info-----\n" ++ show err)
                        Left (STypeError err)   ->
                                Left $  generateError ctx (TmMrg tm1 tm2) 
                                        "Type error on first-part of merge."
                                        ("Check if first part of merge is well-defined. \n-----Further info-----\n" ++ show err)
elaborateInfer ctx (TmBox tm1 tm2)  
                = case elaborateInfer ctx tm1 of
                        Right (ctx', tm1') -> 
                                case elaborateInfer ctx' tm2 of
                                        Right (ty, tm2')        -> 
                                                Right (ty, Box tm1' tm2')
                                        Left (STypeError err)   -> 
                                                Left $ generateError ctx' tm2 
                                                        "Type error on the term inside the box."
                                                        ("Check if the term inside the box is well-defined.\n-----Further info-----\n" ++ show err)
                        Left (STypeError err) -> 
                                Left $ generateError ctx tm1 
                                        "Type error on box construct"
                                        ("Make sure that the environment for box construct is correct.\n-----Further info-----\n" ++ show err)
elaborateInfer ctx (TmIf tm1 tm2 tm3) 
                = case elaborateCheck ctx tm1 Source.TBool of
                        Right tm1'      ->
                                case elaborateInfer ctx tm2 of
                                        Right (ty2, tm2') -> 
                                                case elaborateCheck ctx tm3 ty2 of
                                                        Right tm3'      -> 
                                                                Right (ty2, If tm1' tm2' tm3')
                                                        Left (STypeError err) -> 
                                                                Left $ generateError ctx tm3
                                                                        "Type error on the if statement: types don't match of then and else branch."
                                                                        "Make sure that the types of both branches are same"
                                        Left (STypeError err) ->
                                                Left $ generateError ctx tm2
                                                        "Type error on the then branch of if statement"
                                                        ("Make sure that the term inside `then` is well-defined and sound.\n----Further info-----\n" ++ show err)
                        Left (STypeError err)   ->
                                Left $ generateError ctx tm1
                                                "Type error on condition: condition must be of type Bool"
                                                ("Fix the condition and make sure it is of type Bool.\n-----Further info-----\n" ++ show err)   
elaborateInfer ctx (TmFix (TmLam tA tm)) = 
                case elaborateInfer (Source.TAnd ctx (Source.TArrow tA tA)) (TmLam tA tm) of
                        Right res       -> Right res
                        Left (STypeError err)   
                                        -> Left $ generateError ctx (TmFix (TmLam tA tm)) 
                                                "Type error on fixpoint"
                                                err
elaborateInfer ctx (TmBinOp op tm1 tm2) =
                case elaborateInfer ctx tm1 of
                        Right (ty, tm1') -> 
                                case elaborateCheck ctx tm2 ty of
                                        Right tm2'      -> 
                                                Right (ty, BinOp (elaborateBinaryOp op) tm1' tm2')
                                        Left (STypeError err) ->
                                                Left $ generateError ctx (TmBinOp op tm1 tm2)
                                                        ("Type error on operation `" ++ show op ++ "`. Types on both sides don't match.")
                                                        ("Make sure that types of both operands are same.\n-----Further info-----\n" ++ show err)
                        Left err ->
                                Left $ generateError ctx tm1 
                                        ("Type error on first operand of the operator" ++ show op)
                                        ("First operand has type error.\n-----Further info-----\n" ++ show err)
elaborateInfer ctx (TmUnOp op tm)     =
                case elaborateInfer ctx tm of
                        Right (ty, tm')         -> Right (ty, UnOp (surfaceUnaryToCoreOp op) tm')
                        Left err                -> Left err


elaborateCheck :: Source.Typ -> Tm -> Source.Typ -> Either SourceTypeError Exp
elaborateCheck ctx tm typ       
        = case elaborateInfer ctx tm of
                Right (typ', e')  ->
                        if typ == typ'
                                then Right e'
                                else 
                                        Left  $ generateError ctx tm
                                                        ("Couldn't match expected type \'" ++ show typ ++ "\' with actual type \'" ++ show typ' ++ "\'")
                                                        "Please check your code."
                Left err     -> Left err

elaborate :: Tm -> Maybe Exp
elaborate TmCtx                         = Just Ctx
elaborate TmUnit                        = Just Unit
elaborate (TmLit n)                     = Just $ Lit n
elaborate (TmBool b)                    = Just $ EBool b
elaborate (TmString s)                  = Just $ EString s
elaborate (TmBinOp op tm1 tm2)          = case (elaborateBinaryOp op, elaborate tm1, elaborate tm2) of
                                                (op', Just e1, Just e2) -> Just (BinOp op' e1 e2)
                                                _                       -> Nothing
elaborate (TmUnOp op tm)                = UnOp <$> Just (surfaceUnaryToCoreOp op) <*> elaborate tm
elaborate (TmIf tm1 tm2 tm3)            = If   <$> elaborate tm1        <*> elaborate tm2 <*> elaborate tm3
elaborate (TmMrg tm1 tm2)               = Mrg  <$> elaborate tm1        <*> elaborate tm2
elaborate (TmRec name tm)               = Rec name    <$> elaborate tm
elaborate (TmProj tm n)                 = Proj <$> elaborate tm <*> Just n               
elaborate (TmRProj tm name)             = RProj       <$> elaborate tm <*> Just name
elaborate (TmLam ty tm)                 = Lam <$> elaborateTyp ty <*> elaborate tm
elaborate (TmApp tm1 tm2)               = App <$> elaborate tm1 <*> elaborate tm2
elaborate (TmFix tm)                    = Fix <$> elaborate tm  
elaborate _                             = Nothing