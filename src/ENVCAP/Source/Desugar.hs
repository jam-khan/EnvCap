module ENVCAP.Surface.Desugar where
import ENVCAP.Core.Syntax (Exp(..), Typ(..), Value(..), BinaryOp(..), CompOp(..), ArithOp(..), LogicOp(..), UnaryOp(..))
import ENVCAP.Surface.Syntax (Tm(..), Typ(..), TmBinaryOp(..), TmUnaryOp(..), TmCompOp(..), TmArithOp(..), TmLogicOp(..))


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


surfaceToCore :: Tm -> Exp
surfaceToCore TmCtx                     = Ctx
surfaceToCore TmUnit                    = Unit
surfaceToCore (TmInt n)                 = Lit n
surfaceToCore (TmBool b)                = EBool b
surfaceToCore (TmString s)              = EString s
surfaceToCore (TmBinary op tm1 tm2)     = BinOp (surfaceBinaryToCoreOp op)
                                                (surfaceToCore tm1)
                                                (surfaceToCore tm2)
surfaceToCore (TmUnary op tm)           = UnOp (surfaceUnaryToCoreOp op)
                                                (surfaceToCore tm)
surfaceToCore (TmIf tm1 tm2 tm3)        = If (surfaceToCore tm1)
                                                (surfaceToCore tm2)
                                                (surfaceToCore tm3)

