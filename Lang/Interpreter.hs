module Lang.Interpreter where

import CompReal
import CompOrd
import Solver.Powers
import Solver.Interval
import Solver.FAD
import Solver.Solver
import Lang.Hybrid
import Lang.Parser hiding (Comparator(..))
import qualified Lang.Parser as Lang

import Control.Monad (ap)
import Control.Monad.State.Lazy (evalState, get, put)
import qualified Control.Monad.State.Lazy as MS (State)
import Data.Ratio
import Data.Map as Map (Map, (!), empty, insert)
import qualified Data.Map as Map (map)


--TODO: errors (operation limit exceeded for zeno points, and precision limit exceeded for comparisons)
--the comparisons problem should be studied closer later on (probably need two modes: strict comparison, which guarantees
--correctness but throws PLE, and a parametrized lax mode that assumes equality after specified precision is exhausted)

data RunResult a = Val a | Err String deriving (Functor)
type EState = (Int, Int) --comparison precision, loop iterations
newtype E r a = E { runE :: MS.State EState (Hybrid r (RunResult a)) }

instance (Num r) => Applicative (E r) where
    pure = E . return . pure . Val
    (<*>) = ap

instance (Num r) => Monad (E r) where
    x >>= f = E $ do { es <- get; return $ evalE x es >>= (\y -> evalE (lifted y) es) }
        where lifted (Val x) = f x
              lifted (Err e) = failE e

evalE :: E r a -> EState -> (Hybrid r (RunResult a))
evalE e = evalState (runE e)

failE :: String -> E r a
failE = E . return . end . Err

getCmp :: E r Int
getCmp = E $ fmap (pure . Val . fst) get

decIter :: E r ()
decIter = E $ do
    (cmp, n) <- get
    if n <= 0
    then return $ end $ Err "Iteration limit exhausted"
    else do { put (cmp, n-1); return $ pure $ Val () }


--TODO: change state to list/vector
type PState r = (r, Map Ident r)
type RunnableProgram r = PState r -> E r (PState r)

initial :: (Num r) => PState r
initial = (0, empty)

--Redefined wait and end to update time and return the right type TODO
waitE :: (Num r) => r -> HProgram r (PState r)
waitE d (ti, vars) = for (\t -> (ti + t, vars)) d

endE :: (Num r) => HProgram r (PState r)
endE (ti, vars) = forever (\t -> (ti + t, vars))


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

evalExpr :: (Floating r, Powers r) => Expr -> PState r -> r
evalExpr (Var T) s     = fst s
evalExpr (Var (V v)) s = snd s ! v
evalExpr (Num x) s     = anyFloat x
evalExpr (Func f a) s  = evalFunc f (evalExpr a s)
evalExpr (Op op a b) s = evalOp op (evalExpr a s) (evalExpr b s)

evalComp :: (CompOrd r) => Lang.Comparator -> r -> r -> Int -> Maybe Bool
evalComp Lang.LT  x y = fmap (LT ==) . mCompare x y
evalComp Lang.GT  x y = fmap (GT ==) . mCompare x y
evalComp Lang.LEQ x y = fmap (GT /=) . mCompare x y
evalComp Lang.GEQ x y = fmap (LT /=) . mCompare x y
evalComp Lang.EQ  x y = fmap (EQ ==) . mCompare x y
evalComp Lang.NEQ x y = fmap (EQ /=) . mCompare x y

evalB :: (Floating r, Powers r, CompOrd r) => BTerm -> Int -> PState r -> Maybe Bool
evalB (BConst b) n s   = Just b
evalB (Comp c a b) n s = evalComp c (evalExpr a s) (evalExpr b s) n

--TODO: optimization? identify common expressions and turn them into variables (or do it in the parsing step?)
evalFor :: (Floating r, Powers r) => [(Ident, Expr)] -> PState r -> r -> PState r
evalFor rs (t0, vars) = aux
    where x0 = map ((vars!) . fst) rs
          consts = Map.map con vars
          f t x = map ((`evalExpr` s') . snd) rs
            where s' = (t, foldr (uncurry insert) consts $ zip (map fst rs) x)
          aux dt = (t, foldr (uncurry insert) vars $ zip (map fst rs) $ solve f t0 x0 t)
            where t = t0+dt


fromHybrid :: (Hybrid r a) -> E r a
fromHybrid = E . return . fmap Val

fromMHybrid :: (Hybrid r (Maybe a)) -> E r a
fromMHybrid = E . return . fmap aux
    where aux (Just x) = Val x
          aux Nothing  = Err "Precision exhausted in composition" --TODO: separate error for this

validateT :: r -> E r ()
validateT d = do
    cmp <- getCmp
    if (d >! 0) cmp
    then return ()
    else failE ("Time step is not verifiably positive (comparison precision exhausted): " ++ show d)

compareE :: BTerm -> PState r -> E r Bool
compareE c s = do
    cmp <- getCmp
    case evalB c cmp s of
        Just b  -> return b
        Nothing -> failE "Precision exhausted in comparison"

smth :: RunnableProgram r -> EState -> State r -> Hybrid r (RunResult (PState r))
smth p es s = evalE (p s) es

interpret :: (Floating r, Powers r, CompOrd r) => Program -> RunnableProgram r
interpret Nop s                = return s
interpret (Assign v e) s       = return (fst s, insert v (evalExpr e s) $ snd s)
interpret (For [] (Just t)) s  = let d = evalExpr t s in do { validateT d; fromHybrid $ waitE d s }
interpret (For [] Nothing) s   = fromHybrid $ endE s
interpret (For rs (Just t)) s  = let d = evalExpr t s in do { validateT d; fromHybrid $ for (evalFor rs s) d }
interpret (For rs Nothing) s   = fromHybrid $ forever $ (evalFor rs s)
interpret (IfThenElse c p q) s = do { b <- compareE c s; if b then interpret p s else interpret q s }
interpret (WhileDo c p) s      = do { b <- compareE c s; if b then interpret (Seq p (WhileDo c p)) s else return s }
interpret (Seq p q) s          = do { x; cmp <- getCmp; y; fromMHybrid $ composeComp cmp (evalE . interpret p) (evalE . interpret q) s }
    where x = interpret p s --TODO

run :: (Num r) => RunnableProgram r -> EState -> r -> RunResult (Map Ident r)
run p es = fmap snd . eval (evalE (p initial) es)