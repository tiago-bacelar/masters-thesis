module Lang.Interpreter where

import Utils
import CompReal
import CompOrd
import Solver.Powers
import Solver.Interval
import Solver.FAD
import Solver.Solver
import Lang.Hybrid
import Lang.Parser
import Solver.Poly
import Lang.Expr hiding (E)

import Data.List (sortOn)
import Data.Maybe (isJust, fromMaybe)
import Control.Applicative (liftA2)
import Control.Monad (ap)
import Control.Monad.State as MS (State, runState, evalState, get, put)


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
data EState r = EState { cmp :: Int, iters :: Integer, curDisc :: Maybe ([Ident], PState r, PState r), discs :: DifList ([Ident], PState r, PState r)}
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

addDisc :: (Num r) => Ident -> PState r -> PState r -> E r ()
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


data PState r = PState { time :: r, step :: Int, variables :: [r] }
type RunnableProgram r = PState r -> E r (PState r)

--variables are initialized at 0
initial :: (Num r) => Int -> PState r
initial n = PState { time = 0, step = 0, variables = replicate n 0 }


evalVar :: PState r -> Var -> r
evalVar s T = time s
evalVar s (V v) = variables s !! v

evalBExpr :: (Floating r, Powers r, CompOrd r) => BExpr -> PState r -> E r Bool
evalBExpr e s = do
    cmpPrec <- getCmp
    case maybeEvalBExpr e (evalVar s) cmpPrec of
        Just b -> return b
        Nothing -> failE "Comparison precision exhausted"

evalFor :: (Floating r, Powers r) => [Ident] -> [(Expr, Poly r)] -> PState r -> r -> PState r
evalFor [] _ s = \dt -> s { time = time s + dt }
evalFor is ps s = ans
    where rs = map ((`evalExpr` (evalVar s)) >< id) ps
          --updates the (whole) list of vars by changing only the ones with a differential expression
          updateVars old = map (uncurry fromMaybe) . zip old . maybeIndexes . zip is 
          ans dt = s { time = time s + dt, variables = updateVars (variables s) $ solvePoly rs dt }


incStep :: PState r -> PState r
incStep s = s { step = step s + 1 }

fromHybrid :: (Num r) => (Hybrid r a) -> E r a
fromHybrid h = E $ return $ fmap Val h

validateT :: (Num r, CompOrd r) => r -> E r ()
validateT d = do
    cmpPrec <- getCmp
    if (d >! 0) cmpPrec
    then return ()
    else failE "Time step is not verifiably positive (comparison precision exhausted)"

interpret :: (Floating r, Powers r, CompOrd r) => Program r -> RunnableProgram r
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
        Just (Err e)      -> return h
        Just (Und _ _)    -> error "wtf"
        Nothing           -> return h


run :: (Num r) => RunnableProgram r -> Int -> Int -> Integer -> (Hybrid r (RunResult (Int, [r])), [(r, Int, Int, [Maybe (r, r)])])
run p n cmpPrec nIters = (fmap (fmap (\s -> (step s, variables s))) h, [(time s, step s, step s' ,maybeIndexes [(v, (variables s !! v, variables s' !! v)) | v <- vs]) | (vs, s, s') <- discList])
    where (h, eState) = runState (runE $ p $ initial n) (EState cmpPrec nIters Nothing id)
          discList = (discs eState . maybe id (\d -> (d:)) (curDisc eState)) []

query :: (Num r) => RunnableProgram r -> Int -> Int -> Integer -> r -> RunResult [r]
query p n cmpPrec nIters = fmap snd . (eval $ fst $ run p n cmpPrec nIters)