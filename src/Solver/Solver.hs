{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solver.Solver (solvePoly) where

import Utils
import qualified SnocList as SL
import Limit
import Powers
import CompOrd
import Boundable
import Solver.Coef
import Solver.Poly
import Solver.FAD

import Data.List (unfoldr)
import Data.Either (isLeft)

--TODO: is this memoed? (and does it matter?)
facsFrom2 :: (Num a) => [a]
facsFrom2 = map fromInteger (scanl1 (*) [2..])

generalTerms :: (Fractional a, Powers a) => a -> [a]
generalTerms h = zipWith ($) (id : id : map (flip (/)) facsFrom2) (powers h)


--f can't be strict in its argument. if f tries to
--pattern match on the list, odeDerivs falls into an infinite loop
--as a workaround, consider using lazy pattern matching (~)
--e.g. odeDerivs (\~[x,y] -> [y,-x]) [0,1]  ->   [[0,1], [1,0], [0,-1], ...]
odeDerivs :: ([Dif a] -> [Dif a]) -> [a] -> [[a]]
odeDerivs f x0 = map fromDif x
    where xder = f x
          x = zipWith mkDif x0 xder


solvePoly :: forall r. (Fractional r, Powers r, CompOrd r, Limit r r, Boundable r) => [(r, Poly r)] -> r -> [r]
solvePoly ps = ans . numCoef
    where (x0, exs) = {-traceX "exs" $-} unzip ps
          _exs = map (evalCoef .-. evalPoly . fmap con) exs
          f x = map ($ x) _exs
          --If the taylor series was known to be finite, this would be enough:
          -- ans t = map (foldTree1 (+) . zipWith (*) (generalTerms t)) $ odeDerivs f x0

          --In the general case we can use the known error bound for polynomial ODEs
          --For the theory, check out https://www.sciencedirect.com/science/article/pii/S089812210600352X
          m = max 2 $ maximum $ map degree exs
          bN = compNorm exs
          _M = {-traceX "_M" $-} fromIntegral (m - 1) * bN

          r = 0.5 --must be strictly between 0 and 1. I chose 0.5 to make the accuracy double each step
          dt = {-traceX "dt" $-} fromRational r / (compMax 0.001 _M) --max is used in case _M is zero


          --alpha r = floor (log_2 (1 / r))
          alpha :: Coef r -> Int
          alpha = lg2 . max 2 . lowerBound . recip . compMax 0.001 . abs --max in case r is zero

          --beta r a = ceil (log_2 (max |a| 1 / (1 - r)))
          --for r <= 0.5, this is equivalent to 1 + ceil (log_2 (max |a| 1))
          beta :: r -> Int
          beta = (1+) . lg2 . max 1 . pred . (2*) . upperBound . abs

          --if a term has less than 5 derivatives, shortcuts to a polynomial
          --TODO: shortcut after any finite number of derivs, not just <=5
          --      (and without forcing some number of derivs like here)
          stepDT :: [r] -> [Either (Coef r -> r) r]
          stepDT xi = zipWith (\k0 -> fmap (listLimit . SL.toList . SL.dropOrLast k0 . SL.fromList . scanlTree1 (+) . zipWith (*) genTermsDT)) k0s terms
            where terms = map (\ds -> if lengthGreaterThan 5 ds then Right ds else Left (infStep ds)) $ odeDerivs f xi
                  infStep ds = foldTree1 (+) . zipWith (*) ds . generalTerms . evalCoef
                  k0s = {-traceX "k0 DT" $-} map beta xi
          genTermsDT = generalTerms $ evalCoef dt
          
          stepDelta :: [r] -> Coef r -> [r]
          stepDelta xi = ans2
            where ds = odeDerivs f xi
                  bs = map beta xi
                  ans2 delta = zipWith (listLimit .-. SL.toList .-. SL.getIndexesOrLast) kss terms
                    where genTerms = generalTerms $ evalCoef delta
                          terms = map (SL.fromList . scanlTree1 (+) . zipWith (*) genTerms) ds
                          alp = alpha (delta * _M)
                          kss = map (\b -> [max 0 $ (n + b + 1) `div` alp - 1 | n <- [0..]]) bs

          --whenever possible, we avoid extra operations. Hence the pattern match here
          stepInf :: Coef r -> Integer -> (Coef r -> r) -> r
          stepInf toT 0 g        = g toT
          stepInf toT fromStep g = g (toT - dt * fromInteger fromStep)

          stepInfStep :: Integer -> Integer -> (Coef r -> r) -> r
          stepInfStep toStep fromStep g = case toStep - fromStep of
                                        0 -> g 0
                                        d -> g (dt * fromInteger d)

          steps :: SL.SnocList (Coef r, [Maybe (Integer, Coef r -> r)], Coef r -> [r])
          steps = SL.fromList $ zipWith (\a (b,c) -> (a,b,c)) (fromInteger <$> [0..]) $ unfoldr gen (0, map Right x0)
            where gen :: (Integer,[Either (Integer,Coef r->r) r]) -> Maybe (([Maybe (Integer,Coef r->r)],Coef r->[r]),(Integer,[Either (Integer,Coef r->r) r]))
                  gen (i,s) | all isLeft s = Nothing
                            | otherwise    = Just ((yi, stepDelta xi), (i+1, s'))
                    where xi = map (either (uncurry $ stepInfStep i) id) s
                          yi = map (either Just (const Nothing)) s'
                          s' = zipWith (\eFX eFY -> either Left (const $ either (Left . (i,)) Right eFY) eFX) s (stepDT xi)

          ans :: Coef r -> [r]
          ans t = finalStep $ SL.findOrLast (\(i,_,_) -> maybe True (/=LT) $ asTop $ domCompare i s' 1) $ SL.dropOrLast skip steps
            where s = 2 * t * _M
                  s' = s - 1
                  skip = fromInteger $ max 0 $ lowerBound s
                  finalStep (i, yi, zi) = zipWith (\z -> maybe z (uncurry $ stepInf t)) (zi dt2) yi
                    where dt2 = {-traceX "dt2" $-} t - i * dt --dt2 is picked so that r<=0.5 (even if that requires a negative delta)

