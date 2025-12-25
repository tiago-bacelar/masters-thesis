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

import Data.List (singleton)
import Data.Maybe (fromMaybe)
import GHC.Data.Maybe (rightToMaybe)
import Control.Monad (ap, (>=>))

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



--the discontinuities use the ShowS trick (difference lists) for efficiency
type DifList a = [a] -> [a]
newtype E t s a = E { runE :: EState s -> (CompHybrid t s (Either (Error s) (a, EState s)), DifList (Disc s)) } deriving (Functor)

instance (Num t, CompOrd t) => Applicative (E t s) where
    pure x = E $ \eState -> (pure $ Right (x, eState), id)
    (<*>)  = ap

instance (Num t, CompOrd t) => Monad (E t s) where
    e >>= f = E $ aux . runE e
        where aux (h1, d1) = (fmap fst haux, maybe d1 snd $ endpointCH haux)
                where haux = h1 >>= aux2
                      aux2 (Left err) = return (Left err, d1)
                      aux2 (Right (x, eState)) = fmap (,d1.d2) h2
                        where (h2, d2) = runE (f x) eState

--duration of hybrid must be greater than 0
fromHybrid :: Hybrid t s a -> E t s a
fromHybrid h = E $ \eState -> (Hyb $ smap pure $ (Right . (,eState)) <$> h, id)

--if there was already an error, we keep it. if not, we add the new one
failE :: Error s -> E t s a
failE err = E $ const $ (instantCH $ Left err, id)

getEState :: E t s (EState s)
getEState = E $ \eState -> (instantCH $ Right (eState, eState), id)

setEState :: EState s -> E t s ()
setEState eState = E $ const (instantCH $ Right ((), eState), id)

updateEState :: (EState s -> EState s) -> E t s ()
updateEState f = E $ \eState -> (instantCH $ Right ((), f eState), id)

appendDisc :: Disc s -> E t s ()
appendDisc d = E $ \eState -> (instantCH $ Right ((), eState), (d:))


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

skipDiscE :: (Num t, CompOrd t) => E t s ()
skipDiscE = do
    eState <- getEState
    case curDisc eState of
        Just d  -> updateEState skipDisc >> appendDisc d
        Nothing -> return ()
                            


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



evalFor :: (Floating r, Powers r, CompOrd r, Limit r r, Boundable r) => [Ident] -> [(Expr, Poly r)] -> PState r -> r -> PState r
evalFor [] _ s = \dt -> s { time = time s + dt }
evalFor is ps s = \dt -> s { time = time s + dt, variables = updateVars (variables s) (f dt) }
    where f = solvePoly $ map ((`evalExpr` (evalVar s)) >< id) ps
          --updates the (whole) list of vars by changing only the ones with a differential expression
          updateVars old = map (uncurry fromMaybe) . zip old . maybeIndexes . zip is

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
interpret (For exs ps (Just t)) s   = let d = evalExpr t (evalVar s) in validateT d s >> skipDiscE >> fromHybrid (for (evalFor (map fst exs) ps $ incStep s) d)
interpret (For exs ps Nothing) s    = skipDiscE >> fromHybrid (forever $ evalFor (map fst exs) ps $ incStep s)
interpret (IfThenElse c p q) s      = do { b <- evalCond c s; if b then interpret p s else interpret q s }
interpret (WhileDo c p) s           = do { b <- evalCond c s; if b then decItersE s >> interpret p s >>= interpret (WhileDo c p) else return s }
interpret (Seq p q) s               = interpret p s >>= interpret q



run :: (Num r) => RunnableProgram r -> Int -> Maybe Int -> Maybe Integer -> (CompHybrid r (Step, [r]) (Either (Error (Step, [r])) (Step, [r])), [(r, Step, Step, [Maybe (r, r)])])
run p nVars cmpPrec nIters = (smapCH readPState $ (id >< readPState -|- readPState . fst) <$> h, [(time s, step s, step s', maybeIndexes [(v, (variables s !! v, variables s' !! v)) | v <- vs]) | (vs, s, s') <- discList])
    where (h, discs) = runE (p $ initialPState nVars) (initialEState cmpPrec nIters)
          discList = (discs . maybe id (\d -> (d:)) (endpointCH h >>= rightToMaybe >>= curDisc . snd)) []
          readPState s = (step s, variables s)

query :: (Num r) => RunnableProgram r -> Int -> Maybe Int -> Maybe Integer -> r -> Query [r]
query p nVars cmpPrec nIters = fmap snd . (evalCH $ fst $ run p nVars cmpPrec nIters)
