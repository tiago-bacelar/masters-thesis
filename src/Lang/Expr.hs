module Lang.Expr (
    Ident,
    Var(..),
    Constant(..),
    Function(..),
    Operator(..),
    Expr(..),
    Comparator(..),
    BTerm(..),
    BExpr(..),
    evalConst,
    evalFunc,
    evalOp,
    evalExpr,
    evalComp,
    evalCompInf,
    evalBTerm,
    evalBTermInf,
    evalBExpr,
    evalBExprInf) where

import Powers
import CompOrd hiding (LEQ, GEQ)
import qualified CompOrd as CompOrd

import Prelude hiding (Ordering(..))
import qualified Prelude as P


type Ident = Int

--TODO: sqrt (and other roots?)
data Var = T | V Ident deriving (Show, Eq, Ord)
data Constant = E | PI deriving (Show, Eq, Ord)
data Function = Neg | Exp | Ln | Sin | Cos | Tan deriving (Show, Eq, Ord)
data Operator = Add | Sub | Mult | Div | Pow | Log deriving (Show, Eq, Ord)
data Expr = Var Var
            | Num Rational
            | Const Constant
            | Func Function Expr
            | Op Operator Expr Expr
            | NatPow Expr Int
        deriving (Show, Eq, Ord)

data Comparator = LT | GT | LLT | LGT | LEQ | GEQ deriving (Show)
data BTerm = BConst Bool | Comp Comparator Expr Expr deriving (Show)
data BExpr = Term BTerm | Not BExpr | And BExpr BExpr | Or BExpr BExpr deriving (Show)


evalConst :: (Floating r) => Constant -> r
evalConst E = exp 1
evalConst PI = pi

evalFunc :: (Floating r) => Function -> r -> r
evalFunc Neg = negate
evalFunc Exp = exp
evalFunc Ln  = log
evalFunc Sin = sin
evalFunc Cos = cos
evalFunc Tan = tan

evalOp :: (Floating r) => Operator -> r -> r -> r
evalOp Add  = (+)
evalOp Sub  = (-)
evalOp Mult = (*)
evalOp Div  = (/)
evalOp Pow  = (**)
evalOp Log  = logBase

evalExpr :: (Floating r, Powers r) => Expr -> (Var -> r) -> r
evalExpr (Var v) s      = s v
evalExpr (Const c) _    = evalConst c
evalExpr (Num r) _      = fromRational r
evalExpr (Func f a) s   = evalFunc f (evalExpr a s)
evalExpr (Op op a b) s  = evalOp op (evalExpr a s) (evalExpr b s)
evalExpr (NatPow a n) s = pow (evalExpr a s) n

evalComp :: (CompOrd r) => Comparator -> r -> r -> Int -> Maybe Bool
evalComp LT  x y = mCompare (Top P.LT) . domCompare x y
evalComp GT  x y = mCompare (Top P.GT) . domCompare x y
evalComp LEQ x y = mCompare (Middle CompOrd.LEQ) . domCompare x y
evalComp GEQ x y = mCompare (Middle CompOrd.GEQ) . domCompare x y
evalComp LLT x y = Just . (x <! y)
evalComp LGT x y = Just . (x >! y)

evalCompInf :: (CompOrd r) => Comparator -> r -> r -> Bool
evalCompInf LT  = lesserInf
evalCompInf GT  = greaterInf
evalCompInf LEQ = lesserEqInf
evalCompInf GEQ = greaterEqInf
evalCompInf LLT = lesserInf
evalCompInf LGT = greaterInf

evalBTerm :: (Floating r, Powers r, CompOrd r) => BTerm -> (Var -> r) -> Int -> Maybe Bool
evalBTerm (BConst b)   _ = const (Just b)
evalBTerm (Comp c a b) s = evalComp c (evalExpr a s) (evalExpr b s)

evalBTermInf :: (Floating r, Powers r, CompOrd r) => BTerm -> (Var -> r) -> Bool
evalBTermInf (BConst b)   _ = b
evalBTermInf (Comp c a b) s = evalCompInf c (evalExpr a s) (evalExpr b s)

maybeAnd :: Maybe Bool -> Maybe Bool -> Maybe Bool
maybeAnd (Just a) (Just b) = Just (a && b)
maybeAnd (Just False) _    = Just False
maybeAnd _ (Just False)    = Just False
maybeAnd _ _               = Nothing

maybeOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
maybeOr (Just a) (Just b) = Just (a || b)
maybeOr (Just True) _     = Just True
maybeOr _ (Just True)     = Just True
maybeOr _ _               = Nothing

evalBExpr :: (Floating r, Powers r, CompOrd r) => BExpr -> (Var -> r) -> Int -> Maybe Bool
evalBExpr (Term a)  s cmp = evalBTerm a s cmp
evalBExpr (Not a)   s cmp = fmap not (evalBExpr a s cmp)
evalBExpr (And a b) s cmp = maybeAnd (evalBExpr a s cmp) (evalBExpr b s cmp)
evalBExpr (Or a b)  s cmp = maybeOr  (evalBExpr a s cmp) (evalBExpr b s cmp)

evalBExprInf :: (Floating r, Powers r, CompOrd r) => BExpr -> (Var -> r) -> Bool
evalBExprInf (Term a)  s = evalBTermInf a s
evalBExprInf (Not a)   s = not (evalBExprInf a s)
evalBExprInf (And a b) s = evalBExprInf a s && evalBExprInf b s
evalBExprInf (Or a b)  s = evalBExprInf a s || evalBExprInf b s