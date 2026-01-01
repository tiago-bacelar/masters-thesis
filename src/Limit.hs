{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Limit (Limit(..), errorLimitAux, alternatingSeriesSum, calabreseSum) where

import Utils
import qualified SnocList as SL

import Data.List (singleton)
import Data.Ratio ((%))

class Limit a r where
    --limit of a normalized Cauchy sequence (composed with its modulus of convergence)
    --formally, |f n - r| <= 2^(-n-1) implies limit f = r
    limit :: (Int -> a) -> r
    limit f = listLimit (map f [0..])
    --default limit :: (Fractional a) => (Int -> a) -> r
    --limit f = errorLimit [(f n, fromRational $ 1 % pow2 (n+1)) | n <- [0..]]

    --same as limit, but allows finite lists, in which case the last
    --element of the list is the value of the limit (has zero error)
    listLimit :: [a] -> r
    listLimit xs = limit (SL.indexOrLastMemo $ SL.fromList xs)

    --limit from list of approximation and error pairs
    --the error is assumed to be decreasing (<=), at no specific rate, and
    --the approximations are assumed to be consistent with the previous ones
    --if the list is finite, the last approximation's error is assumed to be zero
    errorLimit :: [(a, a)] -> r
    --This default uses a Real constraint, which most CompReal instances implement as
    --toRational = undefined (or even worse, toRational r = approx r 40 or equivalent)
    --as such, this default isn't useful for Limit r r (and all CompReal instances
    --must override it), but is still used in Limit Rational r
    default errorLimit :: (Real a) => [(a, a)] -> r
    errorLimit = listLimit . errorLimitAux (\n -> (<= (1 % pow2 (n+1))) . toRational)

    --epsilonLimit :: (a -> a) -> r

    {-# MINIMAL (limit | listLimit) #-}

--Translation function between listLimit and errorLimit.
--Receives a predicate function to test the condition e <= 1/2^(n+1)
--The predicate can only return True if the condition is met. If it
--returns False, the condition may be true or false
errorLimitAux :: (Int -> a -> Bool) -> [(a, a)] -> [a]
errorLimitAux p xs = SL.foldr aux (const . singleton . fst) (SL.fromList xs) 0
        where aux (x,e) rec n   | p n e     = x : aux (x,e) rec (n+1)
                                | otherwise = rec n

--To find limits of floating point sequences we just take the first elemnt of the sequence
--whose error is not significant (as in, doesn't change when the two are added together)
instance Limit Rational Double where
    listLimit = errorLimit . (`zip` map ((1%) . pow2) [1..])
    errorLimit = fromRational . fst . SL.findOrLast p . SL.fromList
        where p (x, e) = fromRational (x + e) == (fromRational x :: Double)

instance Limit Double Double where
    listLimit = errorLimit . (`zip` map (fromRational . (1%) . pow2) [1..])
    errorLimit = fst . SL.findOrLast p . SL.fromList
        where p (x, e) = x + e == x



--calculates the sum of an alternating series (series of the form a_0 - a_1 + a_2 - a_3 ...)
--It requires that a_n converges monotonically to 0
--All elements of a should be given as positive (the alternating is built-in)
alternatingSeriesSum :: (Num a, Limit a r) => [a] -> r
alternatingSeriesSum = errorLimit . scanl1 (\(x,_) (y,e) -> (x+y,e)) . aux False
    where aux neg [x] = [(if neg then negate x else x, 0)]
          aux neg (x:y:t) = (if neg then negate x else x, y) : aux (not neg) (y : t)
          aux _ [] = error "alternatingSeriesSum: empty list"

--calculates the sum of an alternating serires with Calabrese's error bound
--It requires that the sequence b_n := a_n − a_(n+1) also converges monotonically to 0
calabreseSum :: (Fractional a, Limit a r) => [a] -> r
calabreseSum = errorLimit . scanl1 (\(x,_) (y,e) -> (x+y,e)) . zipWith (\neg x -> (neg x, x / 2)) (cycle [id, negate])

--johnsonbaughSum :: (Limit a r) => Int -> [a] -> r
--johnsonbaughSum k = TODO   described in https://arxiv.org/pdf/1511.08568
