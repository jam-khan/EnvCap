module ENVCAP.Source.Desugar where
import ENVCAP.Core.Syntax (Exp(..), Typ(..), Value(..), BinaryOp(..), CompOp(..), ArithOp(..), LogicOp(..), UnaryOp(..))
import ENVCAP.Source.Syntax (Tm(..), Typ(..), TmBinaryOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))


desugarBinaryOp :: TmBinaryOp -> BinaryOp
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


desugar :: Tm -> Maybe Exp
desugar TmQuery                   = Just Ctx
desugar TmUnit                    = Just Unit
desugar (TmInt n)                 = Just $ Lit n
desugar (TmBool b)                = Just $ EBool b
desugar (TmString s)              = Just $ EString s
desugar (TmBinary op tm1 tm2)     = case (desugarBinaryOp op, desugar tm1, desugar tm2) of
                                        (op', Just e1, Just e2) -> Just (BinOp op' e1 e2)
                                        _                       -> Nothing
desugar (TmUnary op tm)           = UnOp <$> Just (surfaceUnaryToCoreOp op) <*> desugar tm
desugar (TmIf tm1 tm2 tm3)       = If <$> desugar tm1 <*> desugar tm2 <*> desugar tm3
desugar _                         = Nothing 

