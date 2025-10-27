module Solver.Solver where

import Utils
import Limit
import CompReal (maxCR, scanlTree1)
import CompOrd
import Solver.Powers
import Solver.Poly
import Solver.FAD

import Data.List hiding (singleton)
import Data.Maybe (fromJust)


facs :: (Num a) => [a]
facs = map fromInteger (scanl (*) 1 [1..])

generalTerms :: (Fractional a, Powers a) => a -> [a]
generalTerms h = zipWith (/) (powers h) facs


--f can't be strict in its argument. if f tries to
--unbox its argument, odeDerivs falls into an infinite loop
--as a workaround, consider using lazy pattern matching (~)
odeDerivs :: (Num a) => ([Dif a] -> [Dif a]) -> [a] -> [[a]]
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

--TODO: test this (the coeffs are different in the paper)
scale_coeffs :: (Fractional a) => [a] -> [a]
scale_coeffs = map (maxCR 1 . abs)


solvePoly :: (Fractional r, Powers r, CompOrd r, Limit r r) => [(r, Poly r)] -> r -> [r]
solvePoly ps = ans
    where (x0, exs) = unzip ps
          ds = map (evalPoly . fmap con) exs
          f x = map ($ (x !!)) ds

          m = max 2 $ maximum $ map degree exs
          bN = normCR exs
          _M = fromIntegral (m - 1) * bN

          --r = |_M*dt|
          r = 0.5 --must be strictly between 0 and 1. I chose 0.5 to make the accuracy double each iteration
          auxs = map fromRational $ iterate (r*) (r / (1 - r))
          dt = fromRational r / _M

          --steps[t][j][k] --TODO: steps[t][acc k][j] not do steps after reaching end of derivs
          steps = zip (map fromInteger [0..]) $ iterate (step dt) x0

          ans t = step dt2 xi
            where s = t / dt
                  (i, xi) = fromJust $ find (\(i,_) -> not $ (i <! s) 0) steps
                  dt2 = t - i * dt
          step delta xi = map (errorLimit . replaceLast (\(xij,_) -> (xij,0))) $ zipWith zip terms errs
            where c = scale_coeffs xi
                  gen = generalTerms delta
                  terms = map (scanlTree1 (+) . zipWith (*) gen) $ odeDerivs f xi
                  errs = map ((<$> auxs) . (*)) c