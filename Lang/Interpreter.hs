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
import Control.Monad.State.Lazy as MS (State, runState, evalState, get, put)
import Data.Ratio ((%))
import Data.Map as Map (Map, (!), empty, insert)
import qualified Data.Map as Map (map)


fstOf4 :: (a, b, c, d) -> a
fstOf4 (x,_,_,_) = x


--TODO: make Und a function of desired precision
--TODO: add line and expression of errors

type Error = String
data RunResult a = Val a | Und [a] (Maybe Error) | Err Error deriving (Show, Functor, Foldable)

instance Traversable RunResult where
    sequenceA (Val x)    = fmap Val x
    sequenceA (Und xs e) = fmap (flip Und e) (sequenceA xs)
    sequenceA (Err e)    = pure (Err e)

fromVal :: RunResult a -> a
fromVal (Val x) = x

allVals :: RunResult a -> [a]
allVals (Val x)    = [x]
allVals (Und xs _) = xs
allVals (Err _)    = []

maybeError :: RunResult a -> Maybe Error
maybeError (Und _ e) = e
maybeError (Err e)   = Just e
maybeError _         = Nothing

--EState: (comparison precision, loop iterations, current discontinuity, discontinuities list)
--the discontinuities use the ShowS trick for efficiency
type DifList a = [a] -> [a]
type EState r = (Int, Int, Maybe (PState r, PState r), DifList (PState r, PState r))
newtype E r a = E { runE :: MS.State (EState r) (Hybrid r (RunResult a)) } deriving (Functor)

instance (Num r) => Applicative (E r) where
    pure = E . return . instant . Val
    (<*>) = ap

--This is a dirty instance of Monad, used only for the benefit of the do notation.
--A true instance needs a Comp r restriction to join the Hybrids, which we can't provide
--Instead, this instance only looks at the endpoint of the hybrid x, which is fine as long
--as the hybrid x is instantaneous (has duration 0)
instance (Num r) => Monad (E r) where
    x >>= f = E $ do { v <- runE x; runE (liftF $ mEndpoint v) }
        where liftF (Just (Val x))   = f x
              liftF (Just (Err e))   = failE e
              liftF (Just (Und _ _)) = error "wtf"
              liftF Nothing          = error "wtf"

failE :: (Num r) => Error -> E r a
failE = E . return . instant . Err

getCmp :: (Num r) => E r Int
getCmp = E $ fmap (instant . Val . fstOf4) get

decIter :: (Num r) => E r ()
decIter = E $ do
    (cmp, n, mDisc, discs) <- get
    if n <= 0
    then return $ instant $ Err "Iteration limit exceeded"
    else do 
        put (cmp, n-1, mDisc, discs)
        return $ instant $ Val ()

addDisc :: (Num r) => PState r -> PState r -> E r ()
addDisc s s' = E $ do
    (cmp, n, mDisc, discs) <- get
    put (cmp, n, Just $ maybe (s, s') (\(os, _) -> (os, s')) mDisc, discs)
    return $ instant $ Val ()

skipDisc :: (Num r) => PState r -> E r ()
skipDisc s = E $ do
    (cmp, n, mDisc, discs) <- get
    case mDisc of
        Nothing -> return $ instant $ Val ()
        Just disc -> do
                        put (cmp, n, Nothing, discs . (disc:))
                        return $ instant $ Val ()


--TODO: change state to list/vector
type PState r = (r, Map Ident r)
type RunnableProgram r = PState r -> E r (PState r)

initial :: (Num r) => PState r
initial = (0, empty)

--Redefined wait and end to update time and return the right type
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

evalOp :: (Floating r) => Operator -> r -> r -> r
evalOp Add  = (+)
evalOp Sub  = (-)
evalOp Mult = (*)
evalOp Div  = (/)
evalOp Pow  = (**)
evalOp Log  = logBase

evalExpr :: (Floating r, Powers r) => Expr -> PState r -> r
evalExpr (Var T) s      = fst s
evalExpr (Var (V v)) s  = snd s ! v
evalExpr (Num x) s      = anyFloat x
evalExpr (Func f a) s   = evalFunc f (evalExpr a s)
evalExpr (Op op a b) s  = evalOp op (evalExpr a s) (evalExpr b s)
evalExpr (NatPow a n) s = pow (evalExpr a s) n

evalComp :: (CompOrd r) => Lang.Comparator -> r -> r -> Int -> Maybe Bool
evalComp Lang.LT  x y = fmap (LT ==) . mCompare x y
evalComp Lang.GT  x y = fmap (GT ==) . mCompare x y
evalComp Lang.LEQ x y = fmap (GT /=) . mCompare x y
evalComp Lang.GEQ x y = fmap (LT /=) . mCompare x y
evalComp Lang.EQ  x y = fmap (EQ ==) . mCompare x y
evalComp Lang.NEQ x y = fmap (EQ /=) . mCompare x y
evalComp Lang.LLT x y = Just . (x <! y)
evalComp Lang.LGT x y = Just . (x >! y)

evalBTerm :: (Floating r, Powers r, CompOrd r) => BTerm -> PState r -> E r Bool
evalBTerm (BConst b) s   = return b
evalBTerm (Comp c a b) s = do
    cmp <- getCmp
    case evalComp c (evalExpr a s) (evalExpr b s) cmp of
        Just b  -> return b
        Nothing -> failE "Comparison precision exhausted"

evalBExpr :: (Floating r, Powers r, CompOrd r) => BExpr -> PState r -> E r Bool
evalBExpr (Term b)  s = evalBTerm b s
evalBExpr (Not b)   s = fmap not (evalBExpr b s)
evalBExpr (And b c) s = liftA2 (&&) (evalBExpr b s) (evalBExpr c s)
evalBExpr (Or b c)  s = liftA2 (||) (evalBExpr b s) (evalBExpr c s)


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

interpret :: (Floating r, Powers r, CompOrd r, Show r) => Program -> RunnableProgram r
interpret Nop s                = return s
interpret (Assign v e) s       = let s' = (fst s, insert v (evalExpr e s) $ snd s) in addDisc s s' >> return s'
interpret (For [] (Just t)) s  = let d = evalExpr t s in validateT d >> skipDisc s >> fromHybrid (waitE d s)
interpret (For [] Nothing) s   = skipDisc s >> fromHybrid (endE s)
interpret (For rs (Just t)) s  = let d = evalExpr t s in validateT d >> skipDisc s >> fromHybrid (for (evalFor rs s) d)
interpret (For rs Nothing) s   = skipDisc s >> fromHybrid (forever $ evalFor rs s)
interpret (IfThenElse c p q) s = do { b <- evalBExpr c s; if b then interpret p s else interpret q s }
interpret (WhileDo c p) s      = do { b <- evalBExpr c s; if b then decIter >> interpret (Seq p (WhileDo c p)) s else return s }
interpret (Seq p q) s          = E $ do
    h <- runE (interpret p s)
    (cmp,_,_,_) <- get
    case mEndpoint h of
        Just (Val s2)  -> runE (interpret q s2) >>= return . fmap (either id (\(x,y) -> Und (allVals x ++ allVals y) (maybeError y)) . ($ cmp)) . joinComp h
        Just (Err e)   -> return h
        Just (Und _ _) -> error "an endpoint should never be undecided"
        Nothing        -> return h


run :: (Num r) => RunnableProgram r -> Int -> Int -> (Hybrid r (RunResult (Map Ident r)), [(r, Map Ident r, Map Ident r)])
run p comp iters = (fmap (fmap snd) h, map (\((t,s),(_,s')) -> (t,s,s')) discs)
    where (h, (_,_,mD,ds)) = runState (runE (p initial)) (comp, iters, Nothing, id)
          discs = (ds . maybe id (\d -> (d:)) mD) []

query :: (Num r) => RunnableProgram r -> Int -> Int -> r -> RunResult (Map Ident r)
query p comp iters = eval $ fst $ run p comp iters