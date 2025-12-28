{-# LANGUAGE FlexibleContexts #-}

module CompReal (module CompReal) where

import Utils
import CompOrd
import Limit (Limit)
import qualified Limit

import Data.Ratio


class (Floating r, CompOrd r, Limit Rational r, Limit r r) => CompReal r where
    --receives the desired accuracy and returns an approximation of r
    --formally, |approx n r - r| <= 2^(-n-1)
    approx :: r -> Int -> Rational
    approx r n = (/2) $ uncurry (+) $ bound r n

    --receives the desired accuracy and returns the bound of possible values of r
    --formally, l <= r <= u and u - l <= 2^(-n) where (l,u) = bound n r
    bound :: r -> Int -> (Rational, Rational)
    bound r n = (m - e, m + e)
        where m = approx r n
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


--TODO: slowly increase accuracy if needed?
domCompareDef :: (CompReal r) => r -> r -> Int -> OrderingDomain
domCompareDef x y p = domCompareAux (bound x p) (bound y p)