module Instances where

import Data.List
import Data.Maybe
import Data.Ratio
import Data.Bits
import Control.Applicative

import qualified Data.CDAR as CDAR
import qualified AERN2.Real as AERN2
import qualified ERA.CReal as ERA --available as Data.Numbers.CReal, but that version doesn't export CR, making it kinda useless


pow2 :: Int -> Integer
pow2 = shiftL 1


class (Floating r) => CompReal r where
    --receives a precision and returns an approximation of r
    --formally, |approximate n r - r| <= 2^(-n-1)
    approximate :: r -> Int -> Rational
    approximate r n = (/2) $ uncurry (+) $ bound r n

    --receives a precision and returns the bound of possible values of r
    --formally, l <= r <= u and u - l <= 2^(-n) where (l,u) = bound n r
    bound :: r -> Int -> (Rational, Rational) 
    bound r n = (m - e, m + e)
        where m = approximate r n
              e = 1 % pow2 (n+1)

    --limit of a normalized Cauchy sequence (composed with its modulus of convergence)
    --formally, |approximate (limit s) n - lim s| <= 2^(-n-1)
    limitRational :: (Int -> Rational) -> r
    limitRational f = limit (fromRational . f)

    limit :: (Int -> r) -> r
    limit f = limitRational (\i -> approximate (f (i + 1)) (i + 1))

    --exact should always terminate. If a rational value can't be produced
    --in constant time (or is irrational), Nothing should be returned
    --exact _ = Nothing is a valid implementation, but should be avoided if possible
    --exact (fromRational x) should either be Just x or Nothing
    exact :: r -> Maybe Rational

    {-# MINIMAL (approximate | bound), (limitRational | limit), exact #-}

--TODO: finite lists
--calculates the limit of a sequence through its modulus of convergence
listLimitRational :: (CompReal r) => [Rational] -> (Int -> Int) -> r
listLimitRational s a = limitRational (aux !!)              --TODO: optimize list access?
    where mcList = map a [0..] --to avoid repeated calls
          dif = head mcList : zipWith (-) (tail mcList) mcList
          aux = map head $ scanl (flip drop) s dif

--TODO: finite lists
listLimit :: (CompReal r) => [r] -> (Int -> Int) -> r
listLimit s a = limit (aux !!)                              --same here
    where mcList = map a [0..]
          dif = head mcList : zipWith (-) (tail mcList) mcList
          aux = map head $ scanl (flip drop) s dif

--computes the sum of an infinite series through its modulus of convergence
seriesSumRational :: (CompReal r) => [Rational] -> (Int -> Int) -> r
seriesSumRational (h:t) = listLimitRational (scanl (+) h t) --TODO: for reaaally bad convergence rates, this (i think) is taking up too much memory

seriesSum :: (CompReal r) => [r] -> (Int -> Int) -> r
seriesSum (h:t) = listLimit (scanl (+) h t)


instance CompReal CDAR.CR where

    approximate r n = toRational $ fromJust $ CDAR.centre $ CDAR.require n r

    --limit = CDAR.limCR --this is stupidly bad for expensive sequences because it tries to compactify
                         --the sequence, looking at the first 70 terms to produce the first Approx
    limitRational f = CDAR.CR $ ZipList [CDAR.toApprox i (f i) + CDAR.toApprox i 0 | i <- [0..]]

    exact r | CDAR.exact a = toRational <$> CDAR.centre a
            | otherwise    = Nothing
            where CDAR.CR (ZipList (a : _)) = r


instance CompReal ERA.CReal where
    approximate (ERA.CR r) n = r n % pow2 n --TODO: something fishy here... (correctionPi pi)
    limit f = ERA.CR (\i -> let ERA.CR g = f (i+1) in ERA.round_uk (g (i + 1) % 2))
    exact _ = Nothing


--instance CompReal AERN.RealNumber where
--    approximate 