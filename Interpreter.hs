module Interpreter where

import Prelude hiding (Ordering(..))
import Data.Ratio
import Data.Map (Map, (!), empty, insert)

import Hybrid
import Parser

--TODO: errors (operation limit exceeded for zeno points, and precision limit exceeded for comparisons)
--the comparisons problem should be studied closer later on (probably need two modes: strict comparison, which guarantees
--correctness but throws PLE, and a parametrized lax mode that assumes equality after specified precision is exhausted)

type State r = Map Ident r
type RunnableProgram r = HProgram r (State r) --State r -> Hybrid r (State r)


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
evalOp Pow  = (**) --TODO: pow with Power class
evalOp Log  = logBase

evalExpr :: (Floating r) => Expr -> State r -> r
evalExpr (Var v) s     = s ! v
evalExpr (Num x) s     = anyFloat x
evalExpr (Func f a) s  = evalFunc f (evalExpr a s)
evalExpr (Op op a b) s = evalOp op (evalExpr a s) (evalExpr b s)


evalComp :: (Ord r) => Comparator -> r -> r -> Bool
evalComp LT  = (<)
evalComp GT  = (>)
evalComp LEQ = (<=)
evalComp GEQ = (>=)
evalComp EQ  = (==)
evalComp NEQ = (/=)

evalB :: (Floating r, Ord r) => BTerm -> State r -> Bool
evalB (BConst b) s   = b
evalB (Comp c a b) s = evalComp c (evalExpr a s) (evalExpr b s)

interpret :: (Floating r, Ord r) => Program -> RunnableProgram r
interpret (Assign v e) s       = pure $ insert v (evalExpr e s) s
interpret (For [] t) s         = wait (evalExpr t s) s
interpret (For rs t) s         = undefined --TODO
interpret (IfThenElse c p q) s = if evalB c s then interpret p s else interpret q s
interpret (WhileDo c p) s      = if evalB c s then compose (interpret p) (interpret (WhileDo c p)) s else pure s
interpret (Seq p q) s          = compose (interpret p) (interpret q) s
interpret Nop s                = pure s


run :: RunnableProgram r -> r -> State r
run p = eval (p empty)

query :: RunnableProgram r -> r -> Ident -> r
query p = (!) . run p



playI :: IO (Double -> State Double)
playI = do
       input <- readFile "input.txt"
       case fmap (flip interpret empty) (parseJaguar input) of
              Failed err -> error (show err)
              Ok ans -> return (eval ans)