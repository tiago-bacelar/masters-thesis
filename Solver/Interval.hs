{-# LANGUAGE DefaultSignatures, TypeFamilies #-}

module Solver.Interval where

import Solver.Powers

--represents the default implementation of an interval, using its endpoints
newtype IPair a = IPair {getPair :: (a, a)}

--class of types with a defined concept of intervals
class Intervalable a where
    type Interval a :: *
    (<~>) :: a -> a -> Interval a --interval from ordered endpoints
    lower  :: Interval a -> a
    upper  :: Interval a -> a
    --union        :: Interval a -> Interval a -> Interval a
    --intersection :: Interval a -> Interval a -> Interval a
    --contains     :: Interval a -> a -> Bool

    infix 5 <~>

    type Interval a = IPair a
    default (<~>) :: (Interval a ~ IPair a) => a -> a -> Interval a
    x <~> y = IPair (x, y)
    default lower :: (Interval a ~ IPair a) => Interval a -> a
    lower = fst . getPair
    default upper :: (Interval a ~ IPair a) => Interval a -> a
    upper = snd . getPair

singleton :: (Intervalable a) => a -> Interval a
singleton x = x <~> x --TODO: put inside class?

center, radius :: (Intervalable a, Fractional a) => Interval a -> a
center i = (lower i + upper i) / 2
radius i = (upper i - lower i) / 2

instance Intervalable Double

{-
instance (Intervalable a) => Intervalable [a] where
    type Interval [a] = [Interval a]
    (<~>) = zipWith (<~>)
    upper = map upper
    lower = map lower
-}

ivalCase :: (Num a, Ord a) => IPair a -> b -> b -> b -> b
ivalCase (IPair (l,u)) pos neg zer
  | l >= 0 = pos
  | u <= 0 = neg
  | otherwise = zer

instance (Num a, Ord a) => Num (IPair a) where
    IPair (xl, xu) + IPair (yl, yu) = IPair (xl + yl, xu + yu)
    x@(IPair (xl,xu)) * y@(IPair (yl,yu)) = 
        ivalCase x
            (f (IPair (xl*yl,xu*yu)) (IPair (xu*yl,xl*yu)) (IPair (xu*yl,xu*yu)))
            (f (IPair (xl*yu,xu*yl)) (IPair (xu*yu,xl*yl)) (IPair (xl*yu,xl*yl)))
            (f (IPair (xl*yu,xu*yu)) (IPair (xu*yl,xl*yu)) (IPair (min (xl*yu) (xu*yl),max (xl*yl) (xu*yu))))
        where f a b c = ivalCase y a b c
    negate (IPair (l, u)) = IPair (negate u, negate l)
    abs x@(IPair (l, u)) = ivalCase x x (-x) (IPair (0, max (-l) u))
    signum x = ivalCase x 1 (-1) (error "signum of interval containing 0")
    fromInteger i = let z = fromInteger i in IPair (z, z)

instance (Fractional a, Ord a) => Fractional (IPair a) where
    recip x@(IPair (l, u)) = ivalCase x (IPair (recip u, recip l)) (IPair (recip u, recip l)) (error "signum of interval containing 0")
    fromRational r = let z = fromRational r in IPair (z, z)

instance (Powers a, Ord a) => Powers (IPair a) where
    pow x = (powers x !!)
    powers (IPair (l, u)) = aux
        where aux = IPair (1, 1) : IPair (l, u) : (IPair (l, u)) * (IPair (l, u)) : map (\(IPair (yl, yu)) -> IPair (l2 * yl, u2 * yu)) (tail aux)
              l2 = l * l
              u2 = u * u