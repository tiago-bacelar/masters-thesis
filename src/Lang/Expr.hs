module Lang.Expr where

import Solver.Powers
import CompOrd hiding (OrderingDomain(..))
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
evalComp LT  x y = mCompare (CompOrd.Top P.LT) . domCompare x y
evalComp GT  x y = mCompare (CompOrd.Top P.GT) . domCompare x y
evalComp LEQ x y = mCompare (CompOrd.LEQ) . domCompare x y
evalComp GEQ x y = mCompare (CompOrd.GEQ) . domCompare x y
evalComp LLT x y = Just . (x <! y)
evalComp LGT x y = Just . (x >! y)

evalBTerm :: (Floating r, Powers r, CompOrd r) => BTerm -> (Var -> r) -> Int -> Maybe Bool
evalBTerm (BConst b)   _ = const (Just b)
evalBTerm (Comp c a b) s = evalComp c (evalExpr a s) (evalExpr b s)

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

maybeEvalBExpr :: (Floating r, Powers r, CompOrd r) => BExpr -> (Var -> r) -> Int -> Maybe Bool
maybeEvalBExpr (Term a)  s cmp = evalBTerm a s cmp
maybeEvalBExpr (Not a)   s cmp = fmap not (maybeEvalBExpr a s cmp)
maybeEvalBExpr (And a b) s cmp = maybeAnd (maybeEvalBExpr a s cmp) (maybeEvalBExpr b s cmp)
maybeEvalBExpr (Or a b)  s cmp = maybeOr  (maybeEvalBExpr a s cmp) (maybeEvalBExpr b s cmp)