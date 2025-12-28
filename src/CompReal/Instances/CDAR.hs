{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CompReal.Instances.CDAR (CDAR.CR) where

import Utils
import CompOrd
import CompReal
import Boundable
import Powers
import Limit

import qualified Data.CDAR as CDAR

import Control.Applicative


instance CompReal CDAR.CR where
    bound x n = split (toRational . CDAR.lowerBound) (toRational . CDAR.upperBound) $ CDAR.require n x

instance Limit Rational CDAR.CR where
    listLimit xs = CDAR.CR $ ZipList [CDAR.Approx (round $ x * toRational (pow2 i)) 1 (-i) | (i,x) <- zip [0..] (repeatLast xs)]
    --This implementation keeps all approximations in the list. the problem is, if the
    --approximations converge slowly, the list gets huge and causes a heap overflow
    {-
    errorLimit = CDAR.CR . ZipList . aux 0
        where resources startLimit = ZipList $ iterate bumpLimit $ min 80 startLimit
              bumpLimit p = p * 3 `div` 2
              aux p [(a,_)] = getZipList $ CDAR.toApprox <$> resources p <*> pure a
              aux _ ((a,e):as) = CDAR.Approx (round (a*(toRational $ pow2 p))) 1 (-p) : aux p as
                where p = negate $ min 0 $ logFloor e
    -}

instance Limit CDAR.CR CDAR.CR where
    limit f = CDAR.limCR (f . (max 0) . pred)
    errorLimit = errorLimitDef --TODO??


domCompareA :: CDAR.Approx -> CDAR.Approx -> OrderingDomain
domCompareA a b = domCompareAux (CDAR.lowerBound a, CDAR.upperBound a) (CDAR.lowerBound b, CDAR.upperBound b)

minA :: CDAR.Approx -> CDAR.Approx -> CDAR.Approx
minA a b = CDAR.endToApprox (CDAR.lowerBound a `min` CDAR.lowerBound b) (CDAR.upperBound a `min` CDAR.upperBound b)

maxA :: CDAR.Approx -> CDAR.Approx -> CDAR.Approx
maxA a b = CDAR.endToApprox (CDAR.lowerBound a `max` CDAR.lowerBound b) (CDAR.upperBound a `max` CDAR.upperBound b)

instance CompOrd CDAR.CR where
    domCompare = domCompareDef
    infCompare (CDAR.CR x) (CDAR.CR y) = infCompareAux $ getZipList $ domCompareA <$> x <*> y
    compMin (CDAR.CR x) (CDAR.CR y) = CDAR.CR $ minA <$> x <*> y
    compMax (CDAR.CR x) (CDAR.CR y) = CDAR.CR $ maxA <$> x <*> y

powA :: Int -> CDAR.Approx -> CDAR.Approx
powA _ CDAR.Bottom = CDAR.Bottom
powA n (CDAR.Approx m e s)
    | even n && am <= e = CDAR.Approx ame ame (n*s-1)
    | even n && m < 0   = CDAR.Approx (a+b) (b-a) (n*s-1)
    | otherwise         = CDAR.Approx (a+b) (a-b) (n*s-1)
    where am = abs m
          ame = (am + e)^(n :: Int)
          a = (m + e)^(n :: Int)
          b = (m - e)^(n :: Int)

instance Powers CDAR.CR where
    pow _ 0 = 1
    pow x 1 = x
    pow x n = CDAR.CR $ fmap (powA n) $ CDAR.unCR x

instance Boundable CDAR.CR


instance Show CDAR.CR where
    show = CDAR.showA . CDAR.require 80