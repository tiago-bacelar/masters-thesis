{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Limit where

import Utils
import CompOrd

import Data.Ratio ((%))
import Control.Applicative


class Limit a r where
    --limit of a normalized Cauchy sequence (composed with its modulus of convergence)
    --formally, |f n - r| <= 2^(-n-1) implies limit f = r
    limit :: (Int -> a) -> r

    --limit from list of approximation and error pairs
    --the error is assumed to be decreasing (<=), at no specific rate, and
    --the approximations are assumed to be consistent with the previous ones
    --if the list is finite, the last approximation's error is assumed to be zero
    errorLimit :: [(a, a)] -> r

    --epsilonLimit :: (a -> a) -> r


    default limit :: (Fractional a) => (Int -> a) -> r
    limit f = errorLimit [(f n, fromRational $ 1 % pow2 (n+1)) | n <- [0..]]

    --This default uses a Real constraint, which most CompReal instances implement as
    --toRational = undefined (or even worse, toRational r = approx r 40 or equivalent)
    --as such, this default isn't useful for Limit r r, but is still used in Limit Rational r
    default errorLimit :: (Real a) => [(a, a)] -> r
    errorLimit = limit . errorLimitAux (\n e -> toRational e <= 1 % pow2 (n+1))

    {-# MINIMAL (limit | errorLimit) #-}

--Translation function between limit and errorLimit.
--Receives a predicate function to test the condition e <= 1/2^(n+1)
--The predicate can only return True if the condition is met. If it
--returns False, the condition may be true or false
errorLimitAux :: (Int -> a -> Bool) -> [(a, a)] -> Int -> a
errorLimitAux p l = indexOrLast normalized
        where normalized = aux 0 l
              aux n [(x,_)] = [x]
              aux n ((x,e):xs) | p n e = x : aux (n+1) ((x,e):xs)
                               | otherwise = aux n xs

instance Limit Rational Double where
    errorLimit = fromRational . fst . findOrLast p
        where p (x, e) = fromRational (x + e) == fromRational x

instance Limit Double Double where
    errorLimit = fst . findOrLast p
        where p (x, e) = x + e == x



--calculates the sum of an alternating series (series of the form a_0 - a_1 + a_2 - a_3 ...)
--It requires that a_n converges monotonically to 0
--All elements of a should be given as positive (the alternating is built-in)
alternatingSeriesSum :: (Num a, Limit a r) => [a] -> r
alternatingSeriesSum = errorLimit . scanl1 (\(x,_) (y,e) -> (x+y,e)) . aux False
    where aux neg [x] = [(if neg then negate x else x, 0)]
          aux neg (x:y:t) = (if neg then negate x else x, y) : aux (not neg) (y : t)

--calculates the sum of an alternating serires with Calabrese's error bound
--It requires that the sequence b_n := a_n − a_(n+1) also converges monotonically to 0
calabreseSum :: (Fractional a, Limit a r) => [a] -> r
calabreseSum = errorLimit . scanl1 (\(x,_) (y,e) -> (x+y,e)) . zipWith (\neg x -> (neg x, x / 2)) (cycle [id, negate])

--johnsonbaughSum :: (Limit a r) => Int -> [a] -> r
--johnsonbaughSum k = TODO   described in https://arxiv.org/pdf/1511.08568
