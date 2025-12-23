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
import Data.Maybe
import Data.List (zip4)


instance CompReal CDAR.CR where
    approx r n = toRational $ fromJust $ CDAR.centre $ CDAR.require n r

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


minA :: CDAR.Approx -> CDAR.Approx -> CDAR.Approx
minA a b = CDAR.endToApprox (CDAR.lowerBound a `min` CDAR.lowerBound b) (CDAR.upperBound a `min` CDAR.upperBound b)

maxA :: CDAR.Approx -> CDAR.Approx -> CDAR.Approx
maxA a b = CDAR.endToApprox (CDAR.lowerBound a `max` CDAR.lowerBound b) (CDAR.upperBound a `max` CDAR.upperBound b)

instance CompOrd CDAR.CR where
    domCompare = domCompareDef
    infCompare x y = head $ catMaybes $ getZipList $ CDAR.compareCR x y
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

--return the powers of the approx, starting from power 2
powersA :: CDAR.Approx -> [CDAR.Approx]
powersA CDAR.Bottom = repeat CDAR.Bottom
powersA (CDAR.Approx m e s) = map aux $ tail $ zip4 [1..] (iterate (ame0*) ame0) (iterate (a0*) a0) (iterate (b0*) b0)
    where am = abs m
          ame0 = am + e
          a0 = m + e
          b0 = m - e
          aux (n, ame, a, b)
            | even n && am <= e = CDAR.Approx ame ame (n*s-1)
            | even n && m < 0   = CDAR.Approx (a+b) (b-a) (n*s-1)
            | otherwise         = CDAR.Approx (a+b) (a-b) (n*s-1)

instance Powers CDAR.CR where
    pow _ 0 = 1
    pow x 1 = x
    pow x n = CDAR.CR $ fmap (powA n) $ CDAR.unCR x
    --powers x = 1 : x : (map (CDAR.CR . ZipList) $ transpose $ map powersA $ getZipList $ CDAR.unCR x)
    --this implementation of powers is only (slightly) more efficient when later terms require less precision than earlier terms

instance Boundable CDAR.CR


instance Show CDAR.CR where
    show = CDAR.showA . CDAR.require 80