{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CompReal.Instances.CDAR (CDAR.CR) where

import Utils
import qualified SnocList as SL
import CompOrd
import CompReal
import Boundable
import Powers
import Limit

import qualified Data.CDAR as CDAR

import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Applicative (ZipList(..))

--We can't use CDAR.require because it's wrong (returns approximations with 2 more precision
--than required), but this function does essentially the same
instance CompReal CDAR.CR where
    bound x n = split (toRational . CDAR.lowerBound) (toRational . CDAR.upperBound) goodApprox
        where goodApprox = fromMaybe (error "bound: expected infinite list in CDAR.CR")
                            $ find good $ getZipList $ CDAR.unCR x
              good (CDAR.Approx _ 0 _) = True
              good (CDAR.Approx _ e s) = - s - (lg2 e) > n
              good CDAR.Bottom         = False

instance Limit Rational CDAR.CR where
    listLimit xs = CDAR.CR $ ZipList [CDAR.Approx (round $ x * toRational (pow2 i)) 1 (-i) | (i,x) <- zip [0..] (SL.repeatLast $ SL.fromList xs)]


resources :: SL.SnocList Int
resources = SL.iterate bumpLimit 80
    where bumpLimit n = n * 3 `div` 2

instance Limit CDAR.CR CDAR.CR where
    limit f = CDAR.limCR (f . (max 0) . pred) --internally uses resources too
    
    listLimit xs = CDAR.CR $ ZipList $ SL.foldr aux1 aux2 $ SL.zip3 (SL.iterate succ 0) resources elems
        where elems = SL.getIndexesOrLast (SL.toList resources) (SL.fromList xs)
              aux1 (i,p,x)  = ((getZipList $ CDAR.unCR $ CDAR.scale CDAR.unitError (-p) + x) !! i :)
              aux2 (i,_,x)  = drop i $ getZipList $ CDAR.unCR x

    errorLimit = errorLimitDef


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