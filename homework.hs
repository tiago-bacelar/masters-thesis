
import Data.Number.IReal(IReal, hull, (+-), (-+-), (<!), containedIn, atDecimals, upper, lower, prec)
import Data.Number.IReal.IReal(ir, appr)
import Data.Number.IReal.IntegerInterval(radI)
import Data.Number.IReal.FAD

import Data.Bits(shiftL)
import Data.Maybe(fromMaybe)


pow2 :: Int -> Integer
pow2 = shiftL 1


facs :: (Fractional a, Num a) => [a]
facs = map fromInteger (scanl (*) 1 [1..])

generalTerms h = zipWith (/) (iterate (*h) 1) facs

taylorCoeffs f a = zipWith (/) (derivs f a) facs ++ repeat 0

-- Taylor polynomial of degree n of f at a, evaluated at x
taylorPoly f a n x = sum (zipWith (*) (take (n+1) (derivs f a)) (generalTerms (x-a)))

taylorRem f a n x = deriv n1 f (a -+- x) * (x-a)^n1 / facs !! n1
   where n1 = n+1

-- Taylor expansion of f at a of order n with Lagrange's remainder, evaluated at x.
taylor f a n x = taylorPoly f a n x + taylorRem f a n x






odeDerivs :: (Num a, Num b) => (Dif a -> Dif b -> Dif b) -> a -> b -> [b]
odeDerivs f t0 x0 = fromDif x
    where xder = f (var t0) x
          x = mkDif x0 xder

{-
 x' = f t x, t >= t0
 x t0 = x0
-}

--p is (t0, x0)
--n is the order of the taylor method (how many terms of the taylor expansion to use)
--ps is a list of step sizes, each with a number of steps to perform
--the output is a list of pairs (t, x t) calculated at the specified step sizes
solve :: (Dif IReal -> Dif IReal -> Dif IReal) -> (IReal,IReal) -> Int -> [(IReal,Int)] ->  [(IReal,IReal)] 
solve f p _ [] = [p]
solve f p n ((h,s):ps) = rs ++ solve f (head us) n ps
   where (rs,us) = splitAt s (iterate (g h) p)
         g :: IReal -> (IReal, IReal) -> (IReal, IReal)
         g h (t,x) = (t+h, prec 14 (hull [step (t, lower x), step (t, upper x)]))
         hs :: [IReal]
         hs = generalTerms h
         step :: (IReal, IReal) -> IReal
         step (t,x) = sum (take n $ zipWith (*) (odeDerivs f t x) hs) + err
          where ti = t -+- (t+h)
                xi = bound 0.1 Nothing 
                err = odeDerivs f ti xi !! n * hs !! n

                bound :: IReal -> Maybe IReal -> IReal
                bound rad maybeAns
                 | rad > 100 = error "Cannot verify existence"
                 | i1 `containedIn` i0 `atDecimals` (12+2) = bound (rad/2) (Just i0)
                 | otherwise = fromMaybe (bound (2*rad) Nothing) maybeAns
                  where i0 = (x - rad) -+- (x + rad)
                        i1 = x + h * unDif (f (var ti)) i0

--  (+-)  :: Rational -> Rational -> IReal  --constructs interval from center and radius
--  (-+-) :: IReal -> IReal -> IReal        --constructs interval from endpoints
--  atDecimals  :: (Int -> a) -> Int -> a   --transforms a binary precision function to decimal precision
--  upper,lower :: IReal -> IReal           --narrow intervals at lower/upper bound of input
--  hull  :: [IReal] -> IReal               --interval containing all intervals
--  prec  :: Int -> IReal -> IReal          --interval of width at least 10^(-n) containing x
--  containedIn :: IReal -> IReal -> Precision -> Bool  --whether evaluating the intervals at precision p is sufficient to guarantee x is contained in y





--calculates the element at the intersection of a list of arbitrarily shrinking nested intervals
--if the list is finite, the last interval must be degenerate (must have a single element)
--in this implementation for IReals, the intervals are represented by a single IReal
nestedIntersection :: [IReal] -> IReal
nestedIntersection rs = ir (\p -> last (take (p+1) ans) p)
    where ans = aux 0 (zip [0..] rs)
          aux p [(i, r)] = [\p -> appr r p]
          aux p ((i,r):rs) | radI a < pow2 (i-p+1) = const a : aux (p+1) ((i,r):rs) --TODO: mess with slope of diagonal limit?
                           | otherwise = aux p rs
                        where a = appr r i

--a generic implementation wouldn't be as efficient, because it would have to calculate using Rationals instead of Integers
--nestedIntersection :: (Intervalable r, CompReal r) => [Interval r] -> r

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x = x : maybe [] (iterateM f) (f x)

solve2 :: (Dif IReal -> Dif IReal -> Dif IReal) -> (IReal,IReal) -> IReal -> IReal
solve2 f (t0,x0) = \t -> let (tm,xm) = last ((t0,x0) : takeWhile (flip (<! t) 10 . fst) steps) in step (tm,xm) (t-tm)
    where steps = tail $ iterateM next (t0, x0)
          next (tm,xm) = let h = 1 in Just (tm+h, step (tm, xm) h)  --TODO: select h
          step (tm,xm) h = nestedIntersection $ zipWith (+) (scanl1 (+) terms) (tail errs ++ repeat 0)
            where hs = generalTerms h
                  int_t = tm -+- (tm+h)
                  int_x = bound 1 Nothing
                  terms = zipWith (*) (odeDerivs f tm xm) hs
                  errs = zipWith (*) (odeDerivs f int_t int_x) hs

                  bound rad maybeAns
                   | rad > 1000 = error "Cannot verify existence"
                   | (i1 `containedIn` i0) 10 = bound (rad/2) (Just i0) --tests with precision 10 (3 decimal places)
                   | otherwise = fromMaybe (bound (2*rad) Nothing) maybeAns
                    where i0 = (xm - rad) -+- (xm + rad)
                          i1 = xm + h * unDif (f (var int_t)) i0



--solve3 :: (forall a. Floating a => Dif a -> Dif a -> Dif a) -> (a,a) -> a -> a


{- 
Examples from Tucker:

6.4.2 solve (\t x -> -t*x) (0,0+-1) 3 [(0.1,60),(0.05,100)]
6.4.3 solve (\t x -> x^2) (0,1 -+- 1.25) 3 [(0.05,8),(0.01,30),(0.001,50)]
6.4.4 solve (\t x -> -x^3) (0,1) 4 [(0.1,10),(0.2,15),(0.8,10)]
6.4.5 solve (\t x -> x*(x-1)) (0,1) 7 [(0.1,100)]
6.4.6 solve (\t x -> 5+sin t - x) (1,5+-1) 6 [(0.3,30)]
6.4.7 solve (\t x -> (exp (exp (-t*x)) + 0.01*x^3 + 0.1*x + 2 + 10*cos x+4*sin t - log x)/(0.02*x^3+4*x^2+3*x+4+(x+1)**0.75*0.001*sin (1.5*t*x)+0.001*cos(3.14*t))) (0,3 -+- (3+recip(2^52))) 3 [(0.25,40)]
-}
