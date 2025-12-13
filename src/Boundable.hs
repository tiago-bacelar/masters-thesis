{-# LANGUAGE DefaultSignatures #-}

module Boundable where

import Utils
import Powers
import CompReal


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
powDef :: (Num r, Num a, Powers a, Boundable a) => (r -> Int -> a) -> ((Int -> a) -> r) -> (a -> Int -> a) -> r -> Int -> r
powDef approx cons scale = aux
    where aux _ 0 = 1
          aux x 1 = x
          aux x n = cons f
            where x0 = approx x 0
                  f p = scale (pow xp n) (p - n*q)
                    where xp = approx x q
                          q = p + ceiling (logBase 2 (fromIntegral n) :: Double) 
                                + (n-1) * lg2 (upperBound (abs x0)) + n