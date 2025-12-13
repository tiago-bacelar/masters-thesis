{-# LANGUAGE DefaultSignatures #-}

module Solver.Solver (solvePoly) where

import Utils
import Limit
import Powers
import CompOrd
import Boundable
import Solver.Poly
import Solver.FAD

import Tracing

import Data.List (find)
import Data.Maybe (fromJust)


facs :: (Num a) => [a]
facs = map fromInteger (scanl (*) 1 [1..])

generalTerms :: (Fractional a, Powers a) => a -> [a]
generalTerms h = zipWith (/) (powers h) facs


--f can't be strict in its argument. if f tries to
--unbox its argument, odeDerivs falls into an infinite loop
--as a workaround, consider using lazy pattern matching (~)
odeDerivs :: ([Dif a] -> [Dif a]) -> [a] -> [[a]]
odeDerivs f x0 = map fromDif x
    where xder = f x
          x = zipWith mkDif x0 xder


{-
iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x = x : maybe [] (iterateM f) (f x)

solve :: (Fractional r, Powers r) => (Dif r -> [Dif r] -> [Dif r]) -> r -> [r] -> r -> [r]
solve f t0 x0 = \t -> step (t0,x0) (t-t0) --let (tm,xm) = last ((t0, x0) : takeWhile (flip (<! t) 10 . fst) steps) in step (tm,xm) (t-tm)
    where steps = tail $ iterateM next (t0, x0)
          next (tm,xm) = Nothing --let h = 1 in Just (tm+h, step (tm,xm) h)  --TODO: select optimal h
          step (tm,xm) h = map sum terms --zipWith (\t e -> boundLimit $ zipWith (+) (map singleton $ scanl1 (+) t) (tail e ++ repeat 0)) terms errs
            where hs = generalTerms h
                  --int_t = tm -+- (tm+h)
                  --int_x = bound 1 Nothing
                  terms = map (zipWith (*) hs) (odeDerivs f tm xm)
                  --errs = zipWith (map . (*)) hs (odeDerivs f int_t int_x)

                  --bound rad maybeAns
                  -- | rad > 1000 = error "Cannot verify existence"
                  -- | (i1 `containedIn` i0) 10 = bound (rad/2) (Just i0) --tests with precision 10 (3 decimal places)
                  -- | otherwise = fromMaybe (bound (2*rad) Nothing) maybeAns
                  --  where i0 = (xm - rad) -+- (xm + rad)
                  --        i1 = xm + h * unDif (f (var int_t)) i0

--  > f _ ~[x] = [1]
--  > solve f 0 [0] 1 :: [IReal] -- [1]
-}


solvePoly :: (Fractional r, Powers r, CompOrd r, Limit r r, Boundable r) => [(r, Poly r)] -> r -> [r]
solvePoly ps = ans . numCoef
    where (x0, exs) = traceWith (("exs: "++) . show . map (fmap (bsOrd (-1000) 1000)) . snd) $ unzip ps
          ds = map (evalCoef .-. evalPoly . fmap con) exs
          f x = map ($ x) ds

          m = max 2 $ maximum $ map degree exs
          bN = compNorm exs
          _M = traceCR "_M" $ fromIntegral (m - 1) * bN

          --r = |_M*dt|
          r = 0.5 --must be strictly between 0 and 1. I chose 0.5 to make the accuracy double each iteration
          dt = traceCR "dt" $ fromRational r / _M

          genTermsDT = generalTerms $ evalCoef dt
          stepDT xi = map (Limit.listLimit . uncurry dropOrLast) $ zip ks terms --TODO: optimize list access?
            where terms = map (scanlTree1 (+) . zipWith (*) genTermsDT) $ odeDerivs f xi
                  ks = traceX "ks" $ map ((1+) . lg2 . max 1 . pred . (2*) . upperBound . abs) xi

          {-
          stepDelta delta xi = map (limit . ) $ zip ks terms --TODO: optimize list access?
            where r = traceCR "r" $ _M * delta --assumes delta is positive
                  genTerms = generalTerms delta
                  terms = map (scanlTree1 (+) . zipWith (*) genTermsDT) $ odeDerivs f xi
                  ks = traceX "ks" $ map (\a -> ) xi --TODO: take delta into account
          -}
          --this definition of stepDelta is correct, but can be improved. check the comented version (not done yet)
          stepDelta delta xi = map (Limit.listLimit . uncurry dropOrLast) $ zip ks terms
            where genTerms = generalTerms $ evalCoef delta
                  terms = map (scanlTree1 (+) . zipWith (*) genTerms) $ odeDerivs f xi
                  ks = traceX "ks" $ map ((1+) . lg2 . max 1 . pred . (2*) . upperBound . abs) xi

          --steps[t][j][k] --TODO: steps[t][acc k][j] not do steps after reaching end of derivs
          steps = zip (map fromInteger [0..]) $ iterate stepDT x0
          --traceList "steps" (show . map bounds . snd) $

          ans t = stepDelta dt2 xi
            where s = t / dt
                  (i, xi) = (traceCR "i" >< id) $ fromJust $ find (\(i,_) -> not $ (i <! s) 0) steps
                  dt2 = traceCR "dt2" $ t - i * dt --TODO: ensure 0.25 < r < 0.75 (or some other bound? right now i *think* its between 0 and 0.5(ish))