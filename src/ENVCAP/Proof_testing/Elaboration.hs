{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}
-- {-# HLINT ignore "Use lambda-case" #-}
module ENVCAP.Proof_testing.Elaboration where
import Test.QuickCheck
import Data.Maybe (isNothing)

data STm    =   SCtx                    -- ?
            |   SUnit                   -- unit
            |   SLit Integer            -- lit
            |   SProj STm Integer       -- E.n
            |   SLam STyp STm           -- λ A E
            |   SClos STm STyp STm      -- <E1, λ A E2>
            |   SApp STm STm            -- E1 E2
            |   SBox STm STm            -- E1 ▷ E2
            |   SDMrg STm STm           -- E1 ,, E2 (Dependent)
            |   SNMrg STm STm           -- E1 ,, E2 (Non-dependent)
            |   SRProj STm String       -- E.l
            |   SRec String STm         -- {l = E}
            |   SStruct STyp STm        -- struct A {E}
            |   SMApp STm STm           -- mod application E1 ^ E2
        deriving (Eq, Show)

data STyp   =   STyUnit                 -- unit
            |   STyInt                  -- int
            |   STyAnd STyp STyp        -- ty & ty
            |   STyArrow STyp STyp      -- ty -> ty
            |   STyRecord String STyp   -- {l : ty}
            |   STySig   STyp STyp      -- Sig[A, B]
        deriving (Eq, Show)


genInteger :: Gen Integer
genInteger = arbitrary

getSMrgLen :: STm -> Integer 
getSMrgLen (SDMrg l r)  = getSMrgLen l + getSMrgLen r
getSMrgLen _            = 1

genSMerge :: Gen STm
genSMerge = oneof [
                SDMrg <$> genSTm <*> genSTm,
                SNMrg <$> genSTm <*> genSTm
            ]

genSProj :: Gen STm
genSProj = do
            tm <- SDMrg SUnit <$> genSMerge
            SProj tm <$> choose (0, getSMrgLen tm - 1)

genSAbstraction :: Gen STm 
genSAbstraction = oneof [
                    SLam <$> genSTy <*> genSTm,
                    SClos <$> genSTm <*> genSTy <*> genSTm
                ]

genSRcd :: Gen STm
genSRcd = SRec <$> elements["a", "b", "c", "e", "e"] <*> genSTm

genSDMergeWithRcd :: String -> Gen STm
genSDMergeWithRcd l = oneof [SDMrg  <$> genSDMergeWithRcd l <*> genSTm,
                            SDMrg   <$> genSMerge           <*> genSRcd,
                            SDMrg   <$> genSMerge           <*> (SRec l <$> genSTm),
                            SDMrg   <$> genSDMergeWithRcd l <*> genSRcd]

genSRProj :: Gen STm
genSRProj =  do
                l <- elements ["a", "b", "c", "d", "e"]
                SRProj <$> (SDMrg SUnit <$> genSDMergeWithRcd l) <*> return l


genModule :: Gen STm
genModule = SStruct <$> genSTy <*> genSTm

genSTm :: Gen STm
genSTm = oneof [
                return SCtx,
                return  SUnit,
                SLit    <$> arbitrary,
                genSProj,
                genSAbstraction,
                SApp    <$> genSAbstraction <*> genSTm,
                SBox    <$> genSTm <*> genSTm,
                genSMerge,
                genSRProj,
                genSRcd,
                genModule,
                SMApp   <$> genModule <*> genSTm
                ]

genCtx :: Gen STyp
genCtx = oneof [
        return STyUnit,
        return STyInt,
        STyAnd <$> genCtx <*> genSTy
    ]

genSTy :: Gen STyp
genSTy = oneof  [   
                    return  STyUnit,
                    return  STyInt,
                    STyAnd      <$> genSTy      <*> genSTy,
                    STyArrow    <$> genSTy      <*> genSTy,
                    STyRecord   <$> arbitrary   <*> genSTy,
                    STySig      <$> genSTy      <*> genSTy
                ]

-- Lookup based on indexing
lookupt :: STyp -> Integer -> Maybe STyp
lookupt (STyAnd tA tB) 0
    = Just tB
lookupt (STyAnd tA tB) n
    = lookupt tA (n - 1)
lookupt _ _
    = Nothing

-- checks if l is a label in the typing context
isLabel :: String -> STyp -> Bool
isLabel l (STyRecord label _)
    = l == label
isLabel l (STyAnd tA tB)
    = isLabel l tA || isLabel l tB
isLabel _ _
    = False

-- containment
containment :: STyp -> STyp -> Bool
containment (STyRecord l tA) (STyRecord label typ)
        = l == label && tA == typ
containment (STyRecord l tA) (STyAnd tB tC)
        =   (containment (STyRecord l tA) tB && not (isLabel l tC)) ||
            (containment (STyRecord l tA) tC && not (isLabel l tB))
containment _ _
        = False

-- Lookup based on label
rlookupt :: STyp -> String -> Maybe STyp
rlookupt (STyRecord l t) label
        | l == label = Just t
rlookupt (STyAnd tA tB) label
        = case rlookupt tB label of
                Just t    -> Just t
                Nothing   -> rlookupt tA label
rlookupt _ _
        = Nothing

elaborateInfer :: STyp -> STm -> Maybe (STyp, CTm)
elaborateInfer ctx SCtx         = Just (ctx, Ctx)
elaborateInfer ctx SUnit        = Just (STyUnit, Unit)
elaborateInfer ctx (SLit i)     = Just (STyInt, Lit i)
elaborateInfer ctx (SProj tm i) = elaborateInfer ctx tm >>=
                                    \(tB, e)  -> lookupt tB i >>=
                                        \tA -> return (tA, Proj e i)
elaborateInfer ctx (SLam tA tm) = elaborateInfer (STyAnd ctx tA) tm >>=
                                    \(tB, e)    -> return (STyArrow tA tB, Lam (elaborateTyp tA) e)
{-- Not sure abt closure elaboration rule! careful --}
elaborateInfer ctx (SClos tm1 tA tm2) 
                                = elaborateInfer ctx tm1 >>= 
                                    \(ctx1, e1) -> elaborateInfer (STyAnd ctx1 tA) tm2 >>=
                                        \(tB, e2) -> return (STyArrow tA tB, Box e1 (Lam (elaborateTyp tA) e2))
elaborateInfer ctx (SApp tm1 tm2) 
                                = elaborateInfer ctx tm1 >>=
                                    \case
                                        (STyArrow tA tB, e1)   -> 
                                            elaborateCheck ctx tm2 tA >>=
                                                    \e2   -> return (tB, App e1 e2)
                                        _                       -> Nothing
elaborateInfer ctx (SBox tm1 tm2)
                                = elaborateInfer ctx tm1 >>=
                                    \(ctx1, e1') -> elaborateInfer ctx1 tm2 >>=
                                        \(tA, e2') -> return (tA, Box e1' e2')
elaborateInfer ctx (SDMrg tm1 tm2)
                                = elaborateInfer ctx tm1 >>=
                                    \(tA, e1') -> elaborateInfer (STyAnd ctx tA) tm2 >>=
                                        \(tB, e2')  -> return (STyAnd tA tB, Mrg e1' e2')
elaborateInfer ctx (SNMrg tm1 tm2)
                                = elaborateInfer ctx tm1 >>=
                                    \(tA, e1') -> elaborateInfer ctx tm2 >>=
                                        \(tB, e2') -> 
                                            return (STyAnd tA tB, 
                                                    App (Lam    (elaborateTyp ctx) 
                                                                (Mrg (Box (Proj Ctx 0) e1') (Box (Proj Ctx 1) e2'))) Ctx)
elaborateInfer ctx (SRProj tm l) 
                                = elaborateInfer ctx tm >>=
                                    \(ctx', e') ->
                                        rlookupt ctx' l >>=
                                            \tA  -> if containment (STyRecord l tA) ctx'
                                                        then Just (tA, RProj e' l)
                                                        else Nothing
elaborateInfer ctx (SRec l tm)  = elaborateInfer ctx tm >>=
                                    \(ty, e') -> return (STyRecord l ty, Rec l e')
elaborateInfer ctx (SStruct tA tm)
                                = elaborateInfer (STyAnd STyUnit tA) tm >>=
                                    \(tB, e')   -> return (STySig tA tB, Box Unit (Lam (elaborateTyp tA) e')) 
elaborateInfer ctx (SMApp tm1 tm2)
                                = elaborateInfer ctx tm1 >>=
                                    \case
                                            (STySig tA tB, e1')    -> 
                                                elaborateCheck ctx tm2 tA >>=
                                                    \e2'    -> return (tB, App e1' e2')
                                            _   -> Nothing

elaborateCheck :: STyp -> STm -> STyp -> Maybe CTm
elaborateCheck ctx tm tA = elaborateInfer ctx tm >>= 
                                \(tB, tm') -> if tB == tA   then Just tm' 
                                                            else Nothing

elaborateTyp :: STyp -> CTyp
elaborateTyp STyUnit            = TyUnit
elaborateTyp STyInt             = TyInt
elaborateTyp (STyRecord l ty)   = TyRecord l $ elaborateTyp ty
elaborateTyp (STyAnd tyA tyB)   =
                TyAnd   (elaborateTyp tyA) (elaborateTyp tyB)
elaborateTyp (STyArrow tyA tyB) =
                TyArrow (elaborateTyp tyA) (elaborateTyp tyB)
elaborateTyp (STySig tyA tyB)   =
                TyArrow   (elaborateTyp tyA) (elaborateTyp tyB)

isSValue :: STm -> Bool
isSValue SUnit                = True
isSValue (SLit _)             = True
isSValue (SClos e1 ty e2)     = isSValue e1
isSValue (SDMrg e1 e2)        = isSValue e1 && isSValue e2
isSValue (SNMrg e1 e2)        = isSValue e1 && isSValue e2
isSValue (SRec l e)           = isSValue e
isSValue _                    = False

genSValue :: Gen STm
genSValue = oneof [
                    return SUnit,
                    SLit    <$> arbitrary,
                    SClos   <$> genSValue <*> genSTy <*> genSTm,
                    SDMrg   <$> genSValue <*> genSValue,
                    SNMrg   <$> genSValue <*> genSValue,
                    SRec    <$> arbitrary <*> genSValue
                ]

typeSafeTranslation :: Property
typeSafeTranslation =
    forAll genCtx $ \ctx ->
        forAll genSTm $ \sE ->
            case elaborateInfer ctx sE of
                Just (tA, cE)   -> 
                    case check (elaborateTyp ctx) cE (elaborateTyp tA) of
                        Just tA'    -> True
                        Nothing     -> False
                Nothing         -> discard

-- Lemma value_weaken
prop_value_weaken1 :: Property
prop_value_weaken1 =
    forAll genSValue $ \v ->
        forAll genSTy $ \e ->
            forAll genSTy $ \e' ->
                isSValue v ==>
                    case (elaborateInfer e v, elaborateInfer e' v) of
                        (Just (t, c), Just (t',c')) -> t == t'
                        _                           -> discard

data CTm    =   Ctx                     -- ?
            |   Unit                    -- unit
            |   Lit Integer             -- lit
            |   Proj CTm Integer        -- e.n
            |   Lam CTyp CTm            -- λ A e
            |   Clos CTm CTyp CTm       -- <v, λ A e>
            |   App CTm CTm             -- e1 e2
            |   Mrg CTm CTm             -- e1 ,, e2
            |   Box CTm CTm             -- e1 ▷ e2
            |   RProj CTm String        -- e.l
            |   Rec String CTm          -- {l = e}
        deriving (Eq, Show)

data CTyp   =   TyUnit                  -- unit
            |   TyInt                   -- int
            |   TyAnd CTyp CTyp         -- ty & ty
            |   TyArrow CTyp CTyp       -- ty -> ty
            |   TyRecord String CTyp    -- {l : ty}
        deriving (Eq, Show)

data Val    =   VInt Integer
            |   VUnit
            |   VClos Val CTyp CTm
            |   VMrg  Val Val
            |   VRcd  String Val
        deriving (Eq, Show)

getMrgLen :: CTm -> Integer
getMrgLen (Mrg l r) = getMrgLen l + getMrgLen r
getMrgLen _         = 1

genMerge :: Gen CTm
genMerge = Mrg <$> genTm <*> genTm

genProj  :: Gen CTm
genProj = do
            tm <- Mrg Unit <$> genMerge
            Proj tm <$> choose (0, getMrgLen tm - 1)

genValue :: Gen CTm
genValue    = oneof [return Unit,
                    Lit     <$> arbitrary,
                    Clos    <$> genValue   <*> genTy <*> genTm,
                    Mrg     <$> genValue   <*> genValue,
                    Rec     <$> elements ["a", "b", "c", "d", "e"] <*> genValue]

genAbstraction :: Gen CTm
genAbstraction = oneof [Lam <$> genTy <*> genTm,
                        Clos <$> genValue <*> genTy <*> genTm]

genRcd :: Gen CTm
genRcd = Rec <$> arbitrary <*> genTm

genMergeWithRcd :: String -> Gen CTm
genMergeWithRcd l = oneof [ Mrg     <$> genMergeWithRcd l <*> genTm,
                            Mrg     <$> genMerge          <*> genRcd,
                            Mrg     <$> genMerge          <*> (Rec l <$> genTm),
                            Mrg     <$> genMergeWithRcd l <*> genRcd]

genRProj :: Gen CTm
genRProj = do
            l <- elements ["a", "b", "c", "d", "e"]
            RProj <$> (Mrg Unit <$> genMergeWithRcd l) <*> return l

genTm :: Gen CTm
genTm = oneof  [return Ctx,
                return Unit,
                Lit <$> arbitrary,
                genProj,
                genAbstraction,
                App <$> genAbstraction <*> genTm,
                genMerge,
                Box <$> genTm <*> genTm,
                genRProj,
                genRcd]

genTy :: Gen CTyp
genTy = oneof [ return TyInt,
                return TyUnit,
                TyArrow <$> genTy <*> genTy,
                TyAnd   <$> genTy <*> genTy,
                TyRecord   <$> elements ["a", "b", "c", "d", "e"] <*> genTy]

lookupt' :: CTyp -> Integer -> Maybe CTyp
lookupt' (TyAnd tA tB) 0          = Just tB
lookupt' (TyAnd tA tB) n          = lookupt' tA (n - 1)
lookupt' _ _                      = Nothing

isLabel' :: String -> CTyp -> Bool
isLabel' l (TyRecord label _)     = l == label
isLabel' l (TyAnd tA tB)          = isLabel' l tA || isLabel' l tB
isLabel' _ _                      = False

containment' :: CTyp -> CTyp -> Bool
containment' (TyRecord l tA) (TyRecord label typ)
        = l == label && tA == typ
containment' (TyRecord l tA) (TyAnd tB tC)
        =   (containment' (TyRecord l tA) tB && not (isLabel' l tC)) ||
            (containment' (TyRecord l tA) tC && not (isLabel' l tB))
containment' _ _
        = False

rlookupt' :: CTyp -> String -> Maybe CTyp
rlookupt' (TyRecord l t) label
        | l == label = Just t
rlookupt' (TyAnd tA tB) label
        = case rlookupt' tB label of
            Just t    -> Just t
            Nothing   -> rlookupt' tA label
rlookupt' _ _
        = Nothing

-- Value must have an empty context

isValue :: CTm -> Bool
isValue Unit                = True
isValue (Lit _)             = True
isValue (Clos e1 ty e2)     = isValue e1
isValue (Mrg e1 e2)         = isValue e1 && isValue e2
isValue (Rec l e)           = isValue e
isValue _                   = False

infer :: CTyp -> CTm -> Maybe CTyp
infer ctx Ctx               = Just ctx
infer ctx Unit              = Just TyUnit
infer ctx (Lit i)           = Just TyInt
infer ctx (Proj e n)        = infer ctx e >>=
                                \ctx' -> lookupt' ctx' n
infer ctx (Lam tA e)        = infer (TyAnd ctx tA) e >>=
                                \tB -> Just $ TyArrow tA tB
infer ctx (Clos e1 tA e2)   = if isValue e1
                                then infer ctx e1 >>=
                                        \ctx1 -> infer (TyAnd ctx1 tA) e2 >>=
                                            \tB -> Just $ TyArrow tA tB
                                else Nothing
infer ctx (App e1 e2)       = case infer ctx e1 of
                                    Just (TyArrow tA tB)    ->
                                        check ctx e2 tA >> Just tB
                                    _                       ->
                                        Nothing
infer ctx (Mrg e1 e2)       = infer ctx e1 >>=
                                (\tA -> infer (TyAnd ctx tA) e2 >>=
                                    \tB -> Just $ TyAnd tA tB)
infer ctx (Box e1 e2)       = infer ctx e1 >>=
                                \ctx' -> infer ctx' e2
infer ctx (RProj e l)       = infer ctx e >>=
                                \ctx'-> 
                                    case rlookupt' ctx' l of
                                        Just tA ->
                                            if containment' (TyRecord l tA) ctx'
                                                then Just tA
                                                else Nothing
                                        Nothing -> Nothing
infer ctx (Rec l e)         = TyRecord l <$> infer ctx e

check :: CTyp -> CTm -> CTyp -> Maybe CTyp
check ctx e ty =
    infer ctx e >>= \ty' -> if ty == ty' then Just ty else Nothing

-- step
lookupv :: CTm -> Integer -> Maybe CTm
lookupv (Mrg v1 v2) 0 = Just v2
lookupv (Mrg v1 v2) n = lookupv v1 (n - 1)
lookupv _ _           = Nothing

rlookupv :: CTm -> String -> Maybe CTm
rlookupv (Rec l v) label
    | l == label = Just v
rlookupv (Mrg v1 v2) label =
    case (rlookupv v1 label, rlookupv v2 label) of
        (Just vL, Nothing)      -> Just vL
        (Nothing, Just vR)      -> Just vR
        (_, _)                  -> Nothing
rlookupv _ _ = Nothing

step :: CTm -> CTm -> Maybe CTm
step e tm   = if isValue e then step' e tm else Nothing

step' :: CTm -> CTm -> Maybe CTm
step' v tm  = if isValue tm then Just tm else step'' v tm

step'' :: CTm -> CTm -> Maybe CTm
step'' v Ctx            = Just v                                    -- sctx
step'' v (Proj e1 n)    = if isValue e1
                            then lookupv e1 n                       -- sprojv 
                            else Proj <$> step v e1 <*> Just n      -- sproj
step'' v (Lam tA e)     = Just $ Clos v tA e                        -- sclos
step'' v (App e1 e2)    =
    case (isValue e1, isValue e2) of
        (True,  True)   -> case (e1, e2) of                             -- sbeta
                                (Clos v1 tA e, v2)  -> Just $ Box (Mrg v1 v2) e
                                _                   -> Nothing
        (True,  False)  -> step v e2 >>= \e2' -> return $ App e1 e2'    -- sbapp
        (False, _)      -> step v e1 >>= \e1' -> return $ App e1' e2    -- sbl
step'' v (Mrg e1 e2)    =
    case (isValue e1, isValue e2) of
        (True, False)   -> step (Mrg v e1) e2 >>= \e2' -> return $ Mrg e1 e2'
        (True, True)    -> Nothing -- this should not occur if implemented correctly as this is a value
        (False, _)      -> step v e1 >>= \e1' -> return $ Mrg e1' e2
step'' v (Box e1 e2)    =
    case (isValue e1, isValue e2) of
        (True, True)    -> Just e2
        (True, False)   -> step e1 e2 >>= \e2' -> Just $ Box e1 e2'
        (False, _)      -> step v e1 >>= \e1' -> Just $ Box e1' e2
step'' v (RProj e1 l)    =
    if isValue e1   then rlookupv e1 l
                    else step v e1 >>= \e2 -> return $ RProj e2 l
step'' v (Rec l e)      = step v e >>= \e' -> return $ Rec l e'


-- Lemma value_weaken
prop_value_weaken :: Property
prop_value_weaken =
    forAll genValue $ \v ->
        forAll genTy $ \e ->
            isValue v ==>
            forAll genTy $ \e' ->
                    infer e v == infer e' v

-- Lemma lookupv_value
lookupv_value :: Property
lookupv_value =
    forAll genTm $ \v ->
    forAll genInteger $ \n ->
    let maybeV' = lookupv v n in
        isValue v ==>
            maybe True isValue maybeV'

-- Lemma gpreservation
gpreservation :: Property
gpreservation =
    forAll genTm $ \e ->
        forAll genValue $ \v ->
            case step v e of
                Just e' ->
                    forAll genTy $ \ctx ->
                        case infer ctx e of
                            Just tA ->
                                infer TyUnit v == Just ctx ==>
                                    infer ctx e' == Just tA
                            Nothing -> discard
                Nothing -> discard

-- Lemma preservation
preservation :: Property
preservation =
    forAll genTm $ \e ->
        case infer TyUnit e of
            Just tA ->
                case step Unit e of
                    Just e' ->  infer TyUnit e' == Just tA
                    Nothing ->  discard
            Nothing -> discard

-- Lemma progress
progress :: Property
progress =
    forAll genTm $ \e ->
        case infer TyUnit e of
            Just tA     -> isValue e || (case step Unit e of
                                                Just e'     -> e /= e'
                                                Nothing     -> False)
            Nothing     -> discard

main :: IO()
main = do
    let args = stdArgs { maxSuccess = 300 }
    quickCheckWith args prop_value_weaken
    putStrLn "Lemma 1.1: value_weaken"
    quickCheckWith args lookupv_value
    putStrLn "Lemma 1.2: lookupv_value"
    quickCheckWith args gpreservation
    putStrLn "Lemma 1.3: gpreservation"
    quickCheckWith args preservation
    putStrLn "Lemma 1.4: preservation"
    quickCheckWith args progress
    putStrLn "Lemma 1.4: progress"



-- Good test:
{--
    STyUnit
    SStruct (STySig STyInt (STySig STyInt STyUnit)) (SLit 0)

    STySig (STySig STyInt (STySig STyInt STyUnit)) STyInt
    ~~~~> TyAnd (TyAnd TyInt (TyAnd TyInt TyUnit)) TyInt

    Box Unit (Lam (TyAnd TyInt (TyAnd TyInt TyUnit)) (Lit 0))) 
--}