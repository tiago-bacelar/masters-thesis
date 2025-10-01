module Solver.Solver where

import CompReal
import Solver.Interval
import Solver.Powers
import Solver.Poly
import Solver.FAD

import Data.List hiding (singleton)
import Data.Ratio
import GHC.TypeNats




--TODO: test this (the coeffs are different in the paper)
scale_coeffs :: (Ord a, Num a) => [a] -> [a]
scale_coeffs = map f
    where f ai = max 1 (abs ai)




facs :: (Num a) => [a]
facs = map fromInteger (scanl (*) 1 [1..])

generalTerms :: (Fractional a, Powers a) => a -> [a]
generalTerms h = zipWith (/) (powers h) facs


--f can't be strict in its second argument. if f tries to
--unbox its second argument, odeDerivs falls into an infinite loop
--as a workaround, consider using lazy pattern matching (~)
odeDerivs :: (Num a) => ([Dif a] -> [Dif a]) -> [a] -> [[a]]
odeDerivs f x0 = map fromDif x
    where xder = f x
          x = zipWith mkDif x0 xder

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x = x : maybe [] (iterateM f) (f x)

{-
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

solvePoly :: (Fractional r, Powers r) => [(r, Poly r)] -> r -> [r]
solvePoly ps = \t -> step x0 t
    where (x0, exs) = unzip ps
          ds = map (fmap con) exs
          f x = map (`evalPoly` (x !!)) ds
          step xi ti = map sum terms
            where terms = map (zipWith (*) (generalTerms ti)) (odeDerivs f xi)