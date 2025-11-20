{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Lang.Interpreter (SimNum, Error, RunResult(..), fromVal, allVals, maybeError, RunnableProgram, interpret, run, query) where

import Utils
import Limit
import CompOrd
import CompReal
import Lang.Hybrid
import Lang.Parser
import Solver.Poly
import Solver.Powers
import Solver.Solver
import Lang.Expr hiding (E)

import Data.Maybe (fromMaybe)
import Control.Monad (ap)
import Control.Monad.State as MS (State, runState, get, put)

type SimNum r = (Floating r, Powers r, CompOrd r, Limit r r, Boundable r)

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
fromVal _ = error "fromVal: failed to parse non val RunResult"

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
data EState r = EState { cmp :: Int, iters :: Integer, curDisc :: Maybe ([Ident], PState r, PState r), discs :: DifList ([Ident], PState r, PState r)}
newtype E r a = E { runE :: MS.State (EState r) (Hybrid r (RunResult a)) } deriving (Functor)

instance (Num r) => Applicative (E r) where
    pure = E . return . instant . Val
    (<*>) = ap

--This is a dirty instance of Monad, used only for the benefit of the do notation.
--A true instance needs an Ord r restriction to join the Hybrids, which we can't provide
--Instead, this instance only looks at the endpoint of the hybrid x, which is fine as long
--as the hybrid x is instantaneous (has duration 0)
instance (Num r) => Monad (E r) where
    x >>= f = E $ do { v <- runE x; runE (liftF $ mEndpoint v) }
        where liftF (Just (Val y))   = f y
              liftF (Just (Err e))   = failE e
              liftF (Just (Und _ _)) = error "wtf"
              liftF Nothing          = error "wtf"

initialE :: Int -> Integer -> EState r
initialE cmpPrec nIters = EState { cmp = cmpPrec, iters = nIters, curDisc = Nothing, discs = id }

failE :: (Num r) => Error -> E r a
failE = E . return . instant . Err

getCmp :: (Num r) => E r Int
getCmp = E $ fmap (instant . Val . cmp) get

decIter :: (Num r) => E r ()
decIter = E $ do
    eState <- get
    if iters eState <= 0
    then return $ instant $ Err "Iteration limit exceeded"
    else do 
        put $ eState { iters = iters eState - 1 }
        return $ instant $ Val ()

addDisc :: (Num r) => Ident -> PState r -> PState r -> E r ()
addDisc v s s' = E $ do
    eState <- get
    put $ eState { curDisc = Just $ maybe ([v], s, s') (\(vs, os, _) -> (setInsert v vs, os, s')) (curDisc eState) }
    return $ instant $ Val ()

skipDisc :: (Num r) => E r ()
skipDisc = E $ do
    eState <- get
    case curDisc eState of
        Nothing -> return $ instant $ Val ()
        Just disc -> do
                        put $ eState { curDisc = Nothing, discs = discs eState . (disc:) }
                        return $ instant $ Val ()


data PState r = PState { time :: r, step :: Int, variables :: [r] }
type RunnableProgram r = PState r -> E r (PState r)

--variables are initialPized at 0
initialP :: (Num r) => Int -> PState r
initialP n = PState { time = 0, step = 0, variables = replicate n 0 }

evalVar :: PState r -> Var -> r
evalVar s T = time s
evalVar s (V v) = variables s !! v

evalBExpr :: (Floating r, Powers r, CompOrd r) => BExpr -> PState r -> E r Bool
evalBExpr e s = do
    cmpPrec <- getCmp
    case maybeEvalBExpr e (evalVar s) cmpPrec of
        Just b -> return b
        Nothing -> failE "Comparison precision exhausted"

evalFor :: (Floating r, Powers r, CompOrd r, Limit r r, Boundable r) => [Ident] -> [(Expr, Poly r)] -> PState r -> r -> PState r
evalFor [] _ s = \dt -> s { time = time s + dt }
evalFor is ps s = \dt -> s { time = time s + dt, variables = updateVars (variables s) (f dt) }
    where f = solvePoly $ map ((`evalExpr` (evalVar s)) >< id) ps
          --updates the (whole) list of vars by changing only the ones with a differential expression
          updateVars old = map (uncurry fromMaybe) . zip old . maybeIndexes . zip is


incStep :: PState r -> PState r
incStep s = s { step = step s + 1 }

fromHybrid :: (Hybrid r a) -> E r a
fromHybrid h = E $ return $ fmap Val h

validateT :: (Num r, CompOrd r) => r -> E r ()
validateT d = do
    cmpPrec <- getCmp
    if (d >! 0) cmpPrec
    then return ()
    else failE "Time step is not verifiably positive (comparison precision exhausted)"

interpret :: (SimNum r) => Program r -> RunnableProgram r
interpret Nop s                  = return s
interpret (Assign v e) s         = let s' = s { step = step s + 1, variables = replaceIndex v (evalExpr e (evalVar s)) $ variables s } in addDisc v s s' >> return s'
interpret (For es ps (Just t)) s = let d = evalExpr t (evalVar s) in validateT d >> skipDisc >> fromHybrid (for (evalFor (map fst es) ps $ incStep s) d)
interpret (For es ps Nothing) s  = skipDisc >> fromHybrid (forever $ evalFor (map fst es) ps $ incStep s)
interpret (IfThenElse c p q) s   = do { b <- evalBExpr c s; if b then interpret p s else interpret q s }
interpret (WhileDo c p) s        = do { b <- evalBExpr c s; if b then decIter >> interpret (Seq p (WhileDo c p)) s else return s }
interpret (Seq p q) s            = E $ do
    h <- runE (interpret p s)
    eState <- get
    let cmpPrec = cmp eState
    case mEndpoint h of
        Just (Val s2) -> runE (interpret q s2) >>= return . fmap (either id (\(x,y) -> Und (allVals x ++ allVals y) (maybeError y)) . ($ cmpPrec)) . joinComp h
        Just (Err _)      -> return h
        Just (Und _ _)    -> error "wtf"
        Nothing           -> return h


run :: (Num r) => RunnableProgram r -> Int -> Int -> Integer -> (Hybrid r (RunResult (Int, [r])), [(r, Int, Int, [Maybe (r, r)])])
run p nVars cmpPrec nIters = (fmap (fmap (\s -> (step s, variables s))) h, [(time s, step s, step s' ,maybeIndexes [(v, (variables s !! v, variables s' !! v)) | v <- vs]) | (vs, s, s') <- discList])
    where (h, eState) = runState (runE $ p $ initialP nVars) (initialE cmpPrec nIters)
          discList = (discs eState . maybe id (\d -> (d:)) (curDisc eState)) []

query :: (Num r) => RunnableProgram r -> Int -> Int -> Integer -> r -> RunResult [r]
query p nVars cmpPrec nIters = fmap snd . (eval $ fst $ run p nVars cmpPrec nIters)