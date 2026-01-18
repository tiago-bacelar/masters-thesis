{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Lang.Interpreter (SimNum, Error, Step, RunnableProgram, interpret, run, query) where

import Utils
import Limit
import Powers
import CompOrd
import Boundable
import Solver.Poly
import Solver.Solver
import Lang.Hybrid
import Lang.Parser
import Lang.Expr hiding (E)

import Data.Maybe (fromMaybe)
import GHC.Data.Maybe (rightToMaybe)
import Control.Monad (ap)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Lazy (Writer, writer, runWriter)


type SimNum r = (Floating r, Powers r, CompOrd r, Limit r r, Boundable r)

--TODO: add line and source code of errors



type Error s = (String, s)
type Disc s = ([Ident], s, s)


data EState s = EState { cmp :: Maybe Int
                       , iters :: Maybe Integer
                       , curDisc :: Maybe (Disc s)
                       }

initialEState :: Maybe Int -> Maybe Integer -> EState s
initialEState cmpPrec nIters = EState { cmp = cmpPrec, iters = nIters, curDisc = Nothing }

decIters :: EState s -> EState s
decIters eState = eState { iters = fmap pred (iters eState) }

addDisc :: Ident -> (s, s) -> EState s -> EState s
addDisc v (s, s') eState = eState { curDisc = Just $ maybe ([v], s, s') (\(vs, os, _) -> (setInsert v vs, os, s')) (curDisc eState) }

skipDisc :: EState s -> EState s
skipDisc eState = eState { curDisc = Nothing }



--The discontinuities are basically a difference list, but there is a complication
--Say we wanted all discontinuities until step 10. If the system was infinite and
--there were no discontinuities, getting the first element of the list would be bottom
--To solve this, the discontinuities must be a function that take a max step
--and return the list of discontinuities until that step.
--If you want to filter by time instead of by step, you can use the hybrid
--to get the step at a given time first and then filter by step as before
newtype DifList a = DifList { untilStep :: Maybe Step -> [a] -> [a] }

instance Semigroup (DifList a) where
    d1 <> d2 = DifList (\ms -> untilStep d1 ms . untilStep d2 ms)

instance Monoid (DifList a) where
    mempty = DifList (const id)

singl :: a -> DifList a
singl d = DifList (const (d:))

regStep :: Step -> DifList a
regStep stp = DifList aux
    where aux Nothing       = id
          aux (Just maxStp) | stp <  maxStp = id
                            | otherwise     = const []


newtype E t s a = E { runE :: EState s -> CompHybridT t s (Writer (DifList (Disc s))) (Either (Error s) (a, EState s)) } deriving (Functor)

instance (Num t, CompOrd t) => Applicative (E t s) where
    pure x = E $ \eState -> pure $ Right (x, eState)
    (<*>)  = ap

instance (Num t, CompOrd t) => Monad (E t s) where
    e >>= f = E $ \eState -> do
        endp <- runE e eState
        case endp of
            Left err -> return (Left err)
            Right (x, eState2) -> runE (f x) eState2

--duration of hybrid must be greater than 0
fromHybrid :: Hybrid t s a -> E t s a
fromHybrid h = E $ \eState -> hybridCH $ (Right . (,eState)) <$> h

failE :: Error s -> E t s a
failE err = E $ const $ instantCH $ Left err

getEState :: E t s (EState s)
getEState = E $ \eState -> instantCH $ Right (eState, eState)

--unused
--setEState :: EState s -> E t s ()
--setEState eState = E $ const (instantCH $ Right ((), eState), mempty)

updateEState :: (EState s -> EState s) -> E t s ()
updateEState f = E $ \eState -> instantCH $ Right ((), f eState)

regStepE :: (Num t, CompOrd t) => Step -> E t s ()
regStepE stp = E $ \eState -> lift (writer (Right ((), eState), regStep stp))

appendDisc :: (Num t, CompOrd t) => Disc s -> E t s ()
appendDisc d = E $ \eState -> lift (writer (Right ((), eState), singl d))


validateT :: (Num t, CompOrd t) => t -> s -> E t s ()
validateT d s = do
    eState <- getEState
    case fmap (d >! 0) (cmp eState) of
        Just False  -> failE ("Time step is not verifiably positive (insufficient comparison accuracy)", s)
        _           -> return ()

decItersE :: (Num t, CompOrd t) => s -> E t s ()
decItersE s = do
    eState <- getEState
    case fmap (<= 0) (iters eState) of
        Just True   -> failE ("Iteration limit exceeded", s)
        _           -> updateEState decIters

addDiscE :: Ident -> (s, s) -> E t s ()
addDiscE v (s,s') = updateEState (addDisc v (s,s'))

skipDiscE :: (Num t, CompOrd t) => Step -> E t s ()
skipDiscE stp = do
    eState <- getEState
    case curDisc eState of
        Just d  -> updateEState skipDisc >> appendDisc d
        Nothing -> return ()
    regStepE stp


type Step = Integer
data PState r = PState { time :: r, step :: Step, variables :: [r] }
type RunnableProgram r = PState r -> E r (PState r) (PState r)

assign :: Ident -> r -> PState r -> PState r
assign var val s = s { variables = replaceIndex var val $ variables s }

incStep :: PState r -> PState r
incStep s = s { step = step s + 1 }

--variables are initialized at 0
initialPState :: (Num r) => Int -> PState r
initialPState nVars = PState { time = 0, step = 0, variables = replicate nVars 0 }

evalVar :: PState r -> Var -> r
evalVar s T = time s
evalVar s (V v) = variables s !! v


evalFor :: (SimNum r) => [Ident] -> [(Expr, Poly r)] -> PState r -> r -> PState r
evalFor [] _ s = \dt -> s { time = time s + dt }
evalFor is ps s = \dt -> s { time = time s + dt, variables = updateVars (variables s) (f dt) }
    where f = solvePoly $ map ((`evalExpr` (evalVar s)) >< id) ps
          --updates the (whole) list of vars by changing only the ones with a differential expression
          updateVars old = zipWith fromMaybe old . maybeIndexes . zip is

evalCond :: (Floating r, Powers r, CompOrd r) => BExpr -> PState r -> E r (PState r) Bool
evalCond bExpr s = do
    eState <- getEState
    case cmp eState of
        Nothing -> return $ evalBExprInf bExpr (evalVar s)
        Just n -> case evalBExpr bExpr (evalVar s) n of
                    Just b -> return b
                    Nothing -> failE ("Insufficient comparison accuracy", s)

interpret :: (SimNum r) => Program r -> RunnableProgram r
interpret Nop s                     = return s
interpret (Assign v ex) s           = let s' = incStep $ assign v (evalExpr ex (evalVar s)) s in addDiscE v (s,s') >> return s'
interpret (For exs ps (Just t)) s   = let d = evalExpr t (evalVar s) in validateT d s >> skipDiscE (step s) >> fromHybrid (for (evalFor (map fst exs) ps $ incStep s) d)
interpret (For exs ps Nothing) s    = skipDiscE (step s) >> fromHybrid (forever $ evalFor (map fst exs) ps $ incStep s)
interpret (IfThenElse c p q) s      = do { b <- evalCond c s; if b then interpret p s else interpret q s }
interpret (WhileDo c p) s           = do { b <- evalCond c s; if b then decItersE s >> interpret p s >>= interpret (WhileDo c p) else return s }
interpret (Seq p q) s               = interpret p s >>= interpret q



run :: (Num r) => RunnableProgram r -> Int -> Maybe Int -> Maybe Integer -> (CompHybrid r (Step, [r]) (Either (Error (Step, [r])) (Step, [r])), Maybe Step -> [(r, Step, Step, [Maybe (r, r)])])
run p nVars cmpPrec nIters = (smapCH readPState $ (id >< readPState -|- readPState . fst) <$> h, \mS -> [(time s, step s, step s', take nVars $ maybeIndexes [(v, (variables s !! v, variables s' !! v)) | v <- vs]) | (vs, s, s') <- discList mS])
    where (h, discs) = runWriter $ runCompHybridT $ runE (p $ initialPState nVars) (initialEState cmpPrec nIters)
          discList maxStep = untilStep (discs <> maybe mempty singl (endpointCH h >>= rightToMaybe >>= curDisc . snd)) maxStep []
          readPState s = (step s, variables s)

query :: (Num r) => RunnableProgram r -> Int -> Maybe Int -> Maybe Integer -> r -> Query [r]
query p nVars cmpPrec nIters = fmap snd . (evalCH $ fst $ run p nVars cmpPrec nIters)
