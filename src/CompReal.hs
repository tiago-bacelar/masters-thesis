{-# LANGUAGE FlexibleContexts #-}

module CompReal (module CompReal) where

import Utils
import qualified SnocList as SL
import CompOrd
import Limit (Limit)
import qualified Limit

import Data.Ratio


class (Floating r, CompOrd r, Limit Rational r, Limit r r) => CompReal r where
    --receives the desired accuracy and returns an approximation of x
    --formally, |approx x n - x| <= 2^(-n-1)
    approx :: r -> Int -> Rational
    approx x n = (/2) $ uncurry (+) $ bound x n

    --receives the desired accuracy and returns the bound of possible values of x
    --formally, l <= x <= u and u - l <= 2^(-n) where (l,u) = bound x n
    bound :: r -> Int -> (Rational, Rational)
    bound x n = (m - e, m + e)
        where m = approx x n
              e = 1 % pow2 (n+1)

    {-# MINIMAL (approx | bound) #-}

--unambiguated aliases of the Limit class
cons :: (CompReal r) => (Int -> Rational) -> r
cons = Limit.limit
listCons :: (CompReal r) => [Rational] -> r
listCons = Limit.listLimit
errorCons :: (CompReal r) => [(Rational,Rational)] -> r
errorCons = Limit.errorLimit
limit :: (CompReal r) => (Int -> r) -> r
limit = Limit.limit
listLimit :: (CompReal r) => [r] -> r
listLimit = Limit.listLimit
errorLimit :: (CompReal r) => [(r,r)] -> r
errorLimit = Limit.errorLimit

--a default implementation of listLimit using limit and a CompReal restriction
errorLimitDef :: (CompReal r) => [(r, r)] -> r
errorLimitDef = listLimit . Limit.errorLimitAux p
    where p n = (<= (1 % pow2 (n+1))) . snd . flip bound (n+1)


--A default implementation of domCompare for CompReals
--Both definitions below are correct. The second definition is more efficient when the
--values being compared are far apart, since a top ordering may be achieved using low accuracy
--However, that logic can be more efficiently implemented by each type, so if possible do so
--Elements of type a are only ever compared against other elements obtained with the same accuracy n
domCompareDef :: (Ord a) => (r -> Int -> (a, a)) -> r -> r -> Int -> OrderingDomain
--domCompareDef b x y n = domCompareAux (b x n) (b y n)
domCompareDef b x y n = SL.findOrLast isTop $ (\i -> domCompareAux (b x i) (b y i)) <$> is
    where is = SL.SnocList (takeWhile (<n) $ 0 : iterate (*2) 1) n

--Same as domCompareDef but for open intervals
domCompareDefOpen :: (Ord a) => (r -> Int -> (a, a)) -> r -> r -> Int -> OrderingDomain
domCompareDefOpen b x y n = SL.findOrLast isTop $ (\i -> domCompareAuxOpen (b x i) (b y i)) <$> is
    where is = SL.SnocList (takeWhile (<n) $ 0 : iterate (*2) 1) n