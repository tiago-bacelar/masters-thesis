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

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Control.Monad.State.Lazy (evalState, get, put)
import qualified Control.Monad.State.Lazy as MS (State)
import Data.Ratio
import Data.Map as Map (Map, (!), empty, insert)
import qualified Data.Map as Map (map)

--the comparisons problem should be studied closer later on (probably need two modes: strict comparison, which guarantees
--correctness but throws PLE, and a parametrized lax mode that assumes equality after specified precision is exhausted)
--the Und should also be a function to allow the used to increase the precision of a specific point independently

type Error = String
data RunResult a = Val a | Und [a] (Maybe Error) | Err Error deriving (Show, Functor, Foldable)

instance Traversable RunResult where
    sequenceA (Val x)    = fmap Val x
    sequenceA (Und xs e) = fmap (flip Und e) (sequenceA xs)
    sequenceA (Err e)    = pure (Err e)

allVals :: RunResult a -> [a]
allVals (Val x)    = [x]
allVals (Und xs _) = xs
allVals (Err _)    = []

maybeError :: RunResult a -> Maybe Error
maybeError (Und _ e) = e
maybeError (Err e)   = Just e
maybeError _         = Nothing


type EState = (Int, Int) --comparison precision, loop iterations
newtype E r a = E { runE :: MS.State EState (Hybrid r (RunResult a)) } deriving (Functor)

instance (Num r) => Applicative (E r) where
    pure = E . return . instant . Val
    (<*>) = ap

instance (Num r) => Monad (E r) where
    x >>= f = E $ do { v <- runE x; runE (liftF $ mEndpoint v) }
        where liftF (Just (Val x))   = f x
              liftF (Just (Err e))   = failE e
              liftF (Just (Und _ _)) = error "wtf"
              liftF Nothing          = error "wtf"

evalE :: E r a -> EState -> (Hybrid r (RunResult a))
evalE e = evalState (runE e)

failE :: (Num r) => String -> E r a
failE = E . return . instant . Err

getCmp :: (Num r) => E r Int
getCmp = E $ fmap (instant . Val . fst) get

decIter :: (Num r) => E r ()
decIter = E $ do
    (cmp, n) <- get
    if n <= 0
    then return $ instant $ Err "Iteration limit exceeded"
    else do { put (cmp, n-1); return $ instant $ Val () }


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

evalB :: (Floating r, Powers r, CompOrd r) => BTerm -> PState r -> E r Bool
evalB (BConst b) s   = return b
evalB (Comp c a b) s = do
    cmp <- getCmp
    case evalComp c (evalExpr a s) (evalExpr b s) cmp of
        Just b  -> return b
        Nothing -> failE "Comparison precision exhausted"

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

validateT :: (Num r, CompOrd r, Show r) => r -> E r ()
validateT d = do
    cmp <- getCmp
    if (d >! 0) cmp --TODO: instead of strict comparison we could use a lax comparison, that only
    then return ()  --fails if the time step is verifiably negative (preferrable for small time steps)
    else failE ("Time step is not verifiably positive (comparison precision exhausted): " ++ show d)

interpret :: forall r. (Floating r, Powers r, CompOrd r, Show r) => Program -> RunnableProgram r
interpret Nop s                = return s
interpret (Assign v e) s       = return (fst s, insert v (evalExpr e s) $ snd s)
interpret (For [] (Just t)) s  = let d = evalExpr t s in do { validateT d; fromHybrid $ waitE d s }
interpret (For [] Nothing) s   = fromHybrid $ endE s
interpret (For rs (Just t)) s  = let d = evalExpr t s in do { validateT d; fromHybrid $ for (evalFor rs s) d }
interpret (For rs Nothing) s   = fromHybrid $ forever $ (evalFor rs s)
interpret (IfThenElse c p q) s = do { b <- evalB c s; if b then interpret p s else interpret q s }
interpret (WhileDo c p) s      = do { b <- evalB c s; if b then decIter >> interpret (Seq p (WhileDo c p)) s else return s }
interpret (Seq p q) s          = E $ do
    h <- runE (interpret p s)
    (cmp,_) <- get
    case mEndpoint h of
        Just (Val s2)  -> runE (interpret q s2) >>= return . fmap (either id (\(x,y) -> Und (allVals x ++ allVals y) (maybeError y)) . ($ cmp)) . joinComp h
        Just (Err e)   -> return h
        Just (Und _ _) -> error "an endpoint should never be undecided"
        Nothing        -> return h

run :: (Num r) => RunnableProgram r -> EState -> r -> RunResult (Map Ident r)
run p es = fmap snd . eval (evalE (p initial) es)