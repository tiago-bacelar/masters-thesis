{-# LANGUAGE DefaultSignatures #-}

module Boundable (Boundable(..), lowerBound, upperBound, powDef) where

{-
This module defines class Boundable, which describes numbers which can generate
integer lower and upper bounds for themselves.
This class is useful when writing algorithms that require a lax estimate of a
number (e.g. approx_floor_of_log2 x = integerLogBase 2 (upperBound x) - 1)
These bounds aren't required to have any specific width. Zero width is allowed,
and so are widths larger than one. But do keep in mind that if the width gets
too big, the approximations generated using the bounds might become too inaccurate,
which could impact algorithm performance
-}

import Utils
import Powers
import CompReal (CompReal, bound)


class Boundable r where
    bounds :: r -> (Integer, Integer)

    default bounds :: (CompReal r) => r -> (Integer, Integer)
    bounds = (floor >< ceiling) . flip bound 0


instance Boundable Double where
    bounds x = (floor x, ceiling x)

instance Boundable Integer where
    bounds x = (x,x)


lowerBound :: (Boundable r) => r -> Integer
lowerBound = fst . bounds

upperBound :: (Boundable r) => r -> Integer
upperBound = snd . bounds


--a default implementation of Powers's pow. this code was taken from
--ireal and adapted to work with Integer as well as IntegerInterval
-- (should this be in Powers? yes it should, but sadly there was a dependency
--  circle going on, and I had to put this function here to break it)
powDef :: (Num r, Powers a, Boundable a) => (r -> Int -> a) -> ((Int -> a) -> r) -> (a -> Int -> a) -> r -> Int -> r
powDef approx cons scale = aux
    where aux _ 0 = 1
          aux x 1 = x
          aux x n = cons f
            where x0 = approx x 0
                  f p = scale (pow xp n) (p - n*q)
                    where xp = approx x q
                          q = p + ceiling (logBase 2 (fromIntegral n) :: Double) 
                                + (n-1) * lg2 (upperBound (abs x0)) + n