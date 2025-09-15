module Lang.Interpreter where

import Utils
import CompReal
import CompOrd
import Solver.Powers
import Solver.Interval
import Solver.FAD
import Solver.Solver
import Lang.Hybrid
import Lang.Parser hiding (Comparator(..))
import qualified Lang.Parser as Lang

import Data.List (sortOn)
import Data.Maybe (isJust)
import GHC.Data.Maybe (orElse)
import Control.Applicative (liftA2)
import Control.Monad (ap)
import Control.Monad.State.Lazy as MS (State, runState, evalState, get, put)
import Data.Ratio ((%))


--TODO: make Und a function of desired precision
--TODO: add line and source code of errors

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

--the discontinuities use the ShowS trick (difference lists) for efficiency
--TODO: allow infinite comp precision and infinite iterations
type DifList a = [a] -> [a]
data EState r = EState { cmp :: Int, iters :: Int, curDisc :: Maybe ([Int], PState r, PState r), discs :: DifList ([Int], PState r, PState r)}
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
getCmp = E $ fmap (instant . Val . cmp) get

decIter :: (Num r) => E r ()
decIter = E $ do
    EState cmpPrec nIters mDisc discList <- get
    if nIters <= 0
    then return $ instant $ Err "Iteration limit exceeded"
    else do 
        put $ EState cmpPrec (nIters-1) mDisc discList
        return $ instant $ Val ()

addDisc :: (Num r) => Int -> PState r -> PState r -> E r ()
addDisc v s s' = E $ do
    EState cmpPrec nIters mDisc discList <- get
    put $ EState cmpPrec nIters (Just $ maybe ([v], s, s') (\(vs, os, _) -> (setInsert v vs, os, s')) mDisc) discList
    return $ instant $ Val ()

skipDisc :: (Num r) => E r ()
skipDisc = E $ do
    EState cmpPrec nIters mDisc discList <- get
    case mDisc of
        Nothing -> return $ instant $ Val ()
        Just disc -> do
                        put $ EState cmpPrec nIters Nothing (discList . (disc:))
                        return $ instant $ Val ()


--TODO: change state to list/vector
data PState r = PState {time :: r, step :: Int, variables :: [r]}
type RunnableProgram r = PState r -> E r (PState r)

--variables are initialized at 0
initial :: (Num r) => PState r
initial = PState 0 0 (repeat 0)

--Redefined wait and end to update time and return the right type
waitE :: (Num r) => r -> HProgram r (PState r)
waitE d (PState ti vars i) = for (\t -> PState (ti + t) vars i) d

endE :: (Num r) => HProgram r (PState r)
endE (PState ti vars i) = forever (\t -> PState (ti + t) vars i)


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
evalExpr (Var T) s      = time s
evalExpr (Var (V v)) s  = variables s !! v
evalExpr (Num x) s      = anyFloat x
evalExpr (Func f a) s   = evalFunc f (evalExpr a s)
evalExpr (Op op a b) s  = evalOp op (evalExpr a s) (evalExpr b s)
evalExpr (NatPow a n) s = pow (evalExpr a s) n

evalComp :: (CompOrd r) => Lang.Comparator -> r -> r -> Int -> Maybe Bool
evalComp Lang.LT  x y = mCompare (Top LT) . domCompare x y
evalComp Lang.GT  x y = mCompare (Top GT) . domCompare x y
evalComp Lang.LEQ x y = mCompare (LEQ) . domCompare x y
evalComp Lang.GEQ x y = mCompare (GEQ) . domCompare x y
evalComp Lang.LLT x y = Just . (x <! y)
evalComp Lang.LGT x y = Just . (x >! y)

evalBTerm :: (Floating r, Powers r, CompOrd r) => BTerm -> PState r -> E r (Maybe Bool)
evalBTerm (BConst b) s   = return $ Just b
evalBTerm (Comp c a b) s = getCmp >>= return . evalComp c (evalExpr a s) (evalExpr b s)

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

maybeEvalBExpr :: (Floating r, Powers r, CompOrd r) => BExpr -> PState r -> E r (Maybe Bool)
maybeEvalBExpr (Term b)  s = evalBTerm b s
maybeEvalBExpr (Not b)   s = fmap (fmap not) (maybeEvalBExpr b s)
maybeEvalBExpr (And b c) s = liftA2 maybeAnd (maybeEvalBExpr b s) (maybeEvalBExpr c s)
maybeEvalBExpr (Or b c)  s = liftA2 maybeOr  (maybeEvalBExpr b s) (maybeEvalBExpr c s)

evalBExpr :: (Floating r, Powers r, CompOrd r) => BExpr -> PState r -> E r Bool
evalBExpr e s = do
    ans <- maybeEvalBExpr e s
    case ans of
        Just b -> return b
        Nothing -> failE "Comparison precision exhausted"


--TODO: optimization? identify common subexpressions and turn them into variables (or do it in the parsing step?)
--TODO: memoize polynomial transformation to avoid repeated work (in while loops, for example)
evalFor :: (Floating r, Powers r) => [(Int, Expr)] -> PState r -> r -> PState r
evalFor rs (PState t0 i vars) = ans
    where rs' = sortOn fst rs
          (indexes, exprs) = unzip rs'
          x0 = [v | (v,m) <- zip vars (maybeIndexes rs'), isJust m]
          consts = map con vars

          --updates the (whole) list of vars by changing only the ones with a differential expression (rs)
          updateVars :: [a] -> [a] -> [a]
          updateVars old = map (uncurry orElse) . (`zip` old) . maybeIndexes . zip indexes
          
          f t x = map (`evalExpr` s') exprs
            where s' = PState t i (updateVars consts x)
          ans dt = PState t i (updateVars vars $ solve f t0 x0 t)
            where t = t0 + dt


incStep :: PState r -> PState r
incStep (PState t i vars) = PState t (i + 1) vars

fromHybrid :: (Num r) => (Hybrid r a) -> E r a
fromHybrid h = E $ return $ fmap Val h

validateT :: (Num r, CompOrd r, Show r) => r -> E r ()
validateT d = do
    cmpPrec <- getCmp
    if (d >! 0) cmpPrec
    then return ()
    else failE ("Time step is not verifiably positive (comparison precision exhausted): " ++ show d)

interpret :: (Floating r, Powers r, CompOrd r, Show r) => Program -> RunnableProgram r
interpret Nop s                = return s
interpret (Assign v e) s       = let s' = PState (time s) (step s + 1) (replaceIndex v (evalExpr e s) $ variables s) in addDisc v s s' >> return s'
interpret (For [] (Just t)) s  = let d = evalExpr t s in validateT d >> skipDisc >> fromHybrid (waitE d $ incStep s)
interpret (For [] Nothing) s   = skipDisc >> fromHybrid (endE (incStep s))
interpret (For rs (Just t)) s  = let d = evalExpr t s in validateT d >> skipDisc >> fromHybrid (for (evalFor rs $ incStep s) d)
interpret (For rs Nothing) s   = skipDisc >> fromHybrid (forever $ evalFor rs $ incStep s)
interpret (IfThenElse c p q) s = do { b <- evalBExpr c s; if b then interpret p s else interpret q s }
interpret (WhileDo c p) s      = do { b <- evalBExpr c s; if b then decIter >> interpret (Seq p (WhileDo c p)) s else return s }
interpret (Seq p q) s          = E $ do
    h <- runE (interpret p s)
    eState <- get
    let cmpPrec = cmp eState
    case mEndpoint h of
        Just (Val s2) -> runE (interpret q s2) >>= return . fmap (either id (\(x,y) -> Und (allVals x ++ allVals y) (maybeError y)) . ($ cmpPrec)) . joinComp h
        Just (Err e)      -> return h
        Just (Und _ _)    -> error "wtf"
        Nothing           -> return h


run :: (Num r) => RunnableProgram r -> Int -> Int -> (Hybrid r (RunResult (Int, [r])), [(r, Int, Int, [Maybe (r, r)])])
run p cmpPrec nIters = (fmap (fmap (\s -> (step s, variables s))) h, [(t,i,j,maybeIndexes [(v, (vars !! v, vars' !! v)) | v <- vs]) | (vs, PState t i vars, PState _ j vars') <- discList])
    where (h, eState) = runState (runE $ p initial) (EState cmpPrec nIters Nothing id)
          discList = (discs eState . maybe id (\d -> (d:)) (curDisc eState)) []

query :: (Num r) => RunnableProgram r -> Int -> Int -> r -> RunResult [r]
query p cmpPrec nIters = fmap snd . (eval $ fst $ run p cmpPrec nIters)