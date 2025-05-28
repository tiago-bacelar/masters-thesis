module Lang.Interpreter where

import CompReal
import Solver.Powers
import Solver.Interval
import Solver.FAD
import Solver.Solver
import Lang.Hybrid hiding (wait, end)
import Lang.Parser

import Prelude hiding (Ordering(..))
import Data.Ratio
import Data.Map as Map (Map, (!), empty, insert)
import qualified Data.Map as Map (map)


--TODO: errors (operation limit exceeded for zeno points, and precision limit exceeded for comparisons)
--the comparisons problem should be studied closer later on (probably need two modes: strict comparison, which guarantees
--correctness but throws PLE, and a parametrized lax mode that assumes equality after specified precision is exhausted)

--TODO: change state to list/vector
type State r = (r, Map Ident r)
type RunnableProgram r = HProgram r (State r) --State r -> Hybrid r (State r)

initial :: (Num r) => State r
initial = (0, empty)

--Redefined wait and end since they aren't "pure" (the time var still has to be updated)
wait :: (Num r) => r -> RunnableProgram r
wait d (ti, vars) = for (\t -> (ti + t, vars)) d

end :: (Num r) => RunnableProgram r
end (ti, vars) = forever (\t -> (ti + t, vars))


evalFunc :: (Floating r) => Function -> r -> r
evalFunc Neg = negate
evalFunc Exp = exp
evalFunc Ln  = log
evalFunc Sin = sin
evalFunc Cos = cos
evalFunc Tan = tan

evalOp :: (Floating r, Powers r) => Operator -> r -> r -> r
evalOp Add  = (+)
evalOp Sub  = (-)
evalOp Mult = (*)
evalOp Div  = (/)
evalOp Pow  = (**) --TODO: pow with Powers class
evalOp Log  = logBase

evalExpr :: (Floating r, Powers r) => Expr -> State r -> r
evalExpr (Var T) s     = fst s
evalExpr (Var (V v)) s = snd s ! v
evalExpr (Num x) s     = anyFloat x
evalExpr (Func f a) s  = evalFunc f (evalExpr a s)
evalExpr (Op op a b) s = evalOp op (evalExpr a s) (evalExpr b s)

--TODO: comparison precision
evalComp :: (Ord r) => Comparator -> r -> r -> Bool
evalComp LT  = (<)
evalComp GT  = (>)
evalComp LEQ = (<=)
evalComp GEQ = (>=)
evalComp EQ  = (==)
evalComp NEQ = (/=)

evalB :: (Floating r, Powers r, Ord r) => BTerm -> State r -> Bool
evalB (BConst b) s   = b
evalB (Comp c a b) s = evalComp c (evalExpr a s) (evalExpr b s)

--TODO: optimization? identify common expressions and turn them into variables (or do it in the parsing step?)
evalFor :: (Floating r, Powers r) => [(Ident, Expr)] -> State r -> r -> State r
evalFor rs (t0, vars) = aux
    where x0 = map ((vars!) . fst) rs
          consts = Map.map con vars
          f t x = map ((`evalExpr` s') . snd) rs
            where s' = (t, foldr (uncurry insert) consts $ zip (map fst rs) x)
          aux dt = (t, foldr (uncurry insert) vars $ zip (map fst rs) $ solve f t0 x0 t)
            where t = t0+dt

interpret :: (Floating r, Powers r, Ord r) => Program -> RunnableProgram r
interpret (Assign v e) s       = pure $ (fst s, insert v (evalExpr e s) $ snd s)
interpret (For [] (Just t)) s  = wait (evalExpr t s) s              --TODO: validate t (>= 0)
interpret (For [] Nothing) s   = end s
interpret (For rs (Just t)) s  = for (evalFor rs s) (evalExpr t s)  --TODO: validate t (>= 0)
interpret (For rs Nothing) s   = forever (evalFor rs s)
interpret (IfThenElse c p q) s = if evalB c s then interpret p s else interpret q s
interpret (WhileDo c p) s      = if evalB c s then compose (interpret p) (interpret (WhileDo c p)) s else pure s
interpret (Seq p q) s          = compose (interpret p) (interpret q) s
interpret Nop s                = pure s


run :: (Num r) => RunnableProgram r -> r -> Map Ident r
run p = snd . eval (p initial)

query :: (Num r) => RunnableProgram r -> r -> Ident -> r
query p = (!) . run p