module ENVCAP.Source.Desugar where
import ENVCAP.Core.Syntax as Core
import ENVCAP.Source.Syntax as Source


desugarBinaryOp :: TmBinOp -> BinaryOp
desugarBinaryOp (TmArith arithop)
        = case arithop of
                TmAdd   -> Arith Add
                TmSub   -> Arith Sub
                TmMul   -> Arith Mul
                TmDiv   -> Arith Div
                TmMod   -> Arith Mod
desugarBinaryOp (TmComp compop)
        = case compop of
                TmEql   -> Comp Eql
                TmNeq   -> Comp Neq
                TmLt    -> Comp Lt
                TmLe    -> Comp Le
                TmGt    -> Comp Gt
                TmGe    -> Comp Ge
desugarBinaryOp (TmLogic logicop)
        = case logicop of
                TmAnd   -> Logic And
                TmOr    -> Logic Or

surfaceUnaryToCoreOp :: TmUnaryOp -> UnaryOp
surfaceUnaryToCoreOp TmNot              = Not

-- Types
-- data Typ      =     TUnit                  -- Unit type for empty environment
--                 |   TInt                   -- Integer type
--                 |   TBool                  -- Boolean type
--                 |   TString                -- String type
--                 |   TAnd Typ Typ           -- Intersection type
--                 |   TArrow Typ Typ         -- Arrow type, e.g. A -> B
--                 |   TRecord String Typ     -- Single-Field Record Type
--                 -- Extensions
--                 |   TList  Typ             -- Type for built-in list
--                 |   TSum   Typ Typ         -- Type for sums
--                 |   TPair  Typ Typ         
--                 deriving (Eq, Show)

desugarTyp :: Source.Typ -> Maybe Core.Typ
desugarTyp Source.TUnit                = Just Core.TUnit
desugarTyp Source.TInt                 = Just Core.TInt
desugarTyp Source.TBool                = Just Core.TBool
desugarTyp Source.TString              = Just Core.TString
desugarTyp (Source.TAnd ty1 ty2)       = Core.TAnd <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (Source.TArrow ty1 ty2)     = Core.TArrow             <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (Source.TRecord label ty)   = Core.TRecord label      <$> desugarTyp ty
desugarTyp (Source.TList ty)           = Core.TList              <$> desugarTyp ty 
desugarTyp (Source.TSum ty1 ty2)       = Core.TSum               <$> desugarTyp ty1 <*> desugarTyp ty2
desugarTyp (Source.TPair ty1 ty2)      = Core.TPair              <$> desugarTyp ty1 <*> desugarTyp ty2

desugar :: Tm -> Maybe Exp
desugar TmCtx                   = Just Ctx
desugar TmUnit                  = Just Unit
desugar (TmLit n)               = Just $ Lit n
desugar (TmBool b)              = Just $ EBool b
desugar (TmString s)            = Just $ EString s
desugar (TmBinOp op tm1 tm2)    = case (desugarBinaryOp op, desugar tm1, desugar tm2) of
                                        (op', Just e1, Just e2) -> Just (BinOp op' e1 e2)
                                        _                       -> Nothing
desugar (TmUnOp op tm)          = UnOp <$> Just (surfaceUnaryToCoreOp op) <*> desugar tm
desugar (TmIf tm1 tm2 tm3)      = If <$> desugar tm1 <*> desugar tm2 <*> desugar tm3
desugar (TmMrg tm1 tm2)         = Mrg <$> desugar tm1 <*> desugar tm2
desugar (TmRec name tm)         = Rec name <$> desugar tm
desugar (TmRProj tm name)       = RProj <$> desugar tm <*> Just name
desugar (TmLam typ tm)          = Lam <$> desugarTyp typ <*> desugar tm
desugar _                       = Nothing