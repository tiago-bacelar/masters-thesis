module Solver.Solver where

import CompReal
import Solver.Interval
import Solver.Powers
import Solver.FAD

import Data.List hiding (singleton)
import Data.Ratio
import GHC.TypeNats

facs :: (Num a) => [a]
facs = map fromInteger (scanl (*) 1 [1..])

generalTerms :: (Fractional a, Powers a) => a -> [a]
generalTerms h = zipWith (/) (powers h) facs


--f can't be strict in its second argument. if f tries to
--unbox its second argument, odeDerivs falls into an infinite loop
--as a workaround, consider using lazy pattern matching (~)
odeDerivs :: (Num a, Num b) => (Dif a -> [Dif b] -> [Dif b]) -> a -> [b] -> [[b]]
odeDerivs f t0 x0 = map fromDif x
    where xder = f (var t0) x
          x = zipWith mkDif x0 xder

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x = x : maybe [] (iterateM f) (f x)


-- (CompReal r, Powers r, Ord r, Intervalable r, Num (Interval r))
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


{-
f _ ~[x] = [1]
solve f 0 [0] 1 :: [IReal] -- [1]
-}