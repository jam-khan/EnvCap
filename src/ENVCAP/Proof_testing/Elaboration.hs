{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
elaborateInfer _ _
    = Nothing

elaborateCheck :: STyp -> STm -> STyp -> Maybe CTm
elaborateCheck _ _ _
    = Nothing

{--
    |A| -> A'
    where A is source type and A' is core type
--}

elaborateTyp :: STyp -> CTyp
elaborateTyp STyUnit            = TyUnit
elaborateTyp STyInt             = TyInt
elaborateTyp (STyRecord l ty)   = TyRecord l $ elaborateTyp ty
elaborateTyp (STyAnd tyA tyB)   =
                TyAnd   (elaborateTyp tyA) (elaborateTyp tyB)
elaborateTyp (STyArrow tyA tyB) =
                TyArrow (elaborateTyp tyA) (elaborateTyp tyB)
elaborateTyp (STySig tyA tyB)   =
                TyAnd   (elaborateTyp tyA) (elaborateTyp tyB)

{-- CORE LEVEL TYPE CHECKER --}

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

genInteger :: Gen Integer
genInteger = arbitrary

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
genTm = oneof [ return Ctx,
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
                                \ctx'-> case rlookupt' ctx' l of
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

-- Lemma gpreservation
preservation :: Property
preservation =
    forAll genTm $ \e ->
        case infer TyUnit e of
            Just tA -> 
                case step Unit e of
                    Just e' ->  infer TyUnit e' == Just tA
                    Nothing ->  discard
            Nothing -> discard

main :: IO()
main = do
    let args = stdArgs { maxSuccess = 1000 }
    res <- quickCheckWithResult args prop_value_weaken
    putStrLn $ "Lemma 1.1: Value_weaken" ++ show res

