{-# LANGUAGE TypeFamilies #-}

module Taylor where

import Deriv
import Data.Proxy

--class of types with a defined concept of closed intervals
--The Fractional a restriction is only used for the default implementations, it's not really needed
class (Fractional a) => Intervalable a where
    type Interval a :: *
    (<~>) :: a -> a -> Interval a --closed interval [min(x,y),max(x,y)]
    lower  :: Interval a -> a
    upper  :: Interval a -> a
    center :: Interval a -> a
    radius :: Interval a -> a
    --union        :: Interval a -> Interval a -> Interval a
    --intersection :: Interval a -> Interval a -> Interval a

    upper i = center i + radius i
    lower i = center i - radius i
    center i = (lower i + upper i) / 2
    radius i = (upper i - lower i) / 2
    --TODO: remove center and radius (and Frac a =>) if I end up not using them

    {-# MINIMAL (<~>), (upper, lower | center, radius) #-}

singleton :: (Intervalable a) => a -> Interval a
singleton x = x <~> x --TODO: put inside class?

instance Intervalable Double where
    type Interval Double = (Double, Double)
    x <~> y = (min x y, max x y)
    lower = fst
    upper = snd

instance (Intervalable a) => Intervalable [a] where
    type Interval [a] = [Interval a]
    (<~>) = zipWith (<~>)
    upper = map upper
    lower = map lower
    center = map center
    radius = map radius

ivalCase :: (Num a, Ord a) => (a, a) -> b -> b -> b -> b
ivalCase (l,u) pos neg zer
  | l >= 0 = pos
  | u <= 0 = neg
  | otherwise = zer

instance (Num a, Ord a) => Num (a, a) where
    (xl, xu) + (yl, yu) = (xl + yl, xu + yu)
    x@(xl,xu) * y@(yl,yu) = 
        ivalCase x
            (f (xl*yl,xu*yu) (xu*yl,xl*yu) (xu*yl,xu*yu))
            (f (xl*yu,xu*yl) (xu*yu,xl*yl) (xl*yu,xl*yl))
            (f (xl*yu,xu*yu) (xu*yl,xl*yu) (min (xl*yu) (xu*yl),max (xl*yl) (xu*yu)))
        where f a b c = ivalCase y a b c
    negate (l, u) = (negate u, negate l)
    abs x@(l, u) = ivalCase x x (-x) (0, max (-l) u)
    signum x = ivalCase x 1 (-1) (error "signum of interval containing 0")
    fromInteger i = let z = fromInteger i in (z, z)

instance (Fractional a, Ord a) => Fractional (a, a) where
    recip x@(l, u) = ivalCase x (recip u, recip l) (recip u, recip l) (error "signum of interval containing 0")
    fromRational r = let z = fromRational r in (z, z)

instance (Powers a, Ord a) => Powers (a, a) where
    pow x = (powers x !!)
    powers (l, u) = aux
        where aux = (1, 1) : (l, u) : (l, u) * (l, u) : map (\(yl, yu) -> (l2 * yl, u2 * yu)) (tail aux)
              l2 = l * l
              u2 = u * u


varTerms :: (Powers a) => [a] -> Deriv a
varTerms vars = aux 1 (map powers vars)
    where aux a [] = DNil
          aux a ((p:ps):pss) = let b = a * p in DCons b (aux a (ps:pss)) (aux b pss)

facTerms :: Deriv Integer
facTerms = aux 1 [1..]
    where aux a (x:xs) = DCons a (aux (a * x) xs) (aux a [1..])

--given a Deriv, returns lists of increasing degree of derivative ([[val], [dx, dy], [dx2, dxdy, dy2], ...])
diagonalize :: Deriv a -> [[a]]
diagonalize = map (map val) . takeWhile (not . null) . iterate (>>= filter (not . nil) . step) . return
    where step DNil = []
          step (DCons _ dv1 dv2) = dv1 : (step dv2)



taylorCoeffs :: (Fractional a) => Deriv a -> Deriv a
taylorCoeffs dv = zipDeriv (/) dv (fromInteger <$> facTerms)

taylorTerms :: (Powers a, Fractional b) => (a -> b -> b) -> Deriv b -> [a] -> [b]
taylorTerms mult dv = map sum . diagonalize . terms
    where coeffs = taylorCoeffs dv
          terms xs = zipDeriv mult (varTerms xs) coeffs

taylorRem :: (Powers a, Fractional (Interval b)) => Proxy b -> (a -> Interval b -> Interval b) -> Deriv (Interval b) -> [a] -> [Interval b]
taylorRem _ mult dv = drop 1 . map sum . diagonalize . terms
    where coeffs = taylorCoeffs dv
          terms xs = zipDeriv mult (varTerms xs) coeffs


--given a Deriv representing a function at point 0 and an input X, returns its taylor
--series as a list, where element n is the summation of the first n terms of the series
--the input X's length should correspond to the dimensionality of the Deriv
taylor :: (Powers a, Fractional b) => (a -> b -> b) -> Deriv b -> [a] -> [b]
taylor mult dv = scanl1 (+) . taylorTerms mult dv



cena :: (Powers a, Fractional b, Fractional (Interval b), Intervalable b) => (a -> b -> b) -> (a -> Interval b -> Interval b) -> Deriv b -> Deriv (Interval b) -> [a] -> [Interval b]
cena mult imult dv dvi = aux
    where coeffs  = taylorCoeffs dv
          icoeffs = taylorCoeffs dvi
          aux xs = zipWith (+) terms iterms --TODO: scanl1 intersection
            where terms = map singleton $ scanl1 (+) $ map sum $ diagonalize $ zipDeriv mult vart coeffs
                  iterms = drop 1 $ map sum $ diagonalize $ zipDeriv imult vart icoeffs
                  vart = varTerms xs

teste :: (Intervalable a, Fractional a, Fractional (Interval a), Powers a, Powers (Interval a)) => a -> [Interval a]
teste x = cena (*) (\x -> (singleton x *)) (f $ var 0 a) (f $ var 0 (a <~> x)) [x - a]
    where a = 0
          f x = pow x 4 - 4 * x + 3


{-
x :: Deriv Double
x = var 0 1
xi :: Deriv (Interval Double)
xi = var 0 (1, 1.5)

f :: (Powers a, Fractional a) => Deriv a -> Deriv a
f x = recip x

ans :: [Interval Double]
ans = cena (*) (\x -> (singleton x *)) (f x) (f xi) [0.5]

ansrem :: [Interval Double]
ansrem = taylorRem (Proxy :: Proxy Double) (\x -> (singleton x *)) (f xi) [0.5 :: Double]
-}