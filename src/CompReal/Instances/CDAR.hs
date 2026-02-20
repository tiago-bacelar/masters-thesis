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
import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import Control.Applicative (ZipList(..))

good :: Int -> CDAR.Approx -> Bool
good _ CDAR.Bottom         = False
good _ (CDAR.Approx _ 0 _) = True
good n (CDAR.Approx _ e s) = - s - (lg2 e) > n

--We can't use CDAR.require because it's wrong (returns approximations with 2 more precision
--than required), but this function does essentially the same
goodApprox :: CDAR.CR -> Int -> CDAR.Approx
goodApprox x n = fromMaybe (error "CDAR.CR bound: expected infinite list in CDAR.CR")
                    $ find (good n) $ getZipList $ CDAR.unCR x

instance CompReal CDAR.CR where
    bound = convert .-. goodApprox
        where convert CDAR.Bottom = error "CDAR.CR bound: could not find good approximation"
              convert (CDAR.Approx m e s) = (fromInteger (m-e) * d, fromInteger (m+e) * d)
                where d = if s < 0 then 1 % pow2 (-s) else fromInteger (pow2 s)

--CDAR doesn't export this, so we have to redefine it
resources :: [Int]
resources = iterate bumpLimit 80
    where bumpLimit n = n * 3 `div` 2

instance Limit Rational CDAR.CR where
    --This implementation was copied from CDAR.limCR and adapted for Rationals and lists
    listLimit xs = CDAR.CR $ ZipList $ map aux $ zip resources $ SL.repeatLast elems
        where elems     = SL.getIndexesOrLast resources (SL.fromList xs)
              aux (p,x) = CDAR.Approx (round $ x * toRational (pow2 p)) 1 (-p)

    {-
    This one is the same but uses all elements of xs instead of skipping according
    to resources. It is much more efficient when xs is slow to compute, but otherwise
    messes with CDAR's expectations of precision in its internal representation of CR.
    -}
    --listLimit xs = CDAR.CR $ ZipList [CDAR.Approx (round $ x * toRational (pow2 p)) 1 (-p) | (p,x) <- zip [0..] (SL.repeatLast $ SL.fromList xs)]

instance Limit CDAR.CR CDAR.CR where
    limit f = CDAR.limCR (f . (max 0) . pred) --internally uses resources too
    
    listLimit xs = CDAR.CR $ ZipList $ SL.foldr aux1 aux2 $ SL.zip3 (SL.iterate succ 0) (SL.fromList resources) elems
        where elems = SL.getIndexesOrLast resources (SL.fromList xs)
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
    domCompare x y n = SL.findOrLast isTop $ SL.zipOrLastWith domCompareA (aux x) (aux y)
        where aux = SL.takeUntil (good n) . getZipList . CDAR.unCR
    infCompare (CDAR.CR x) (CDAR.CR y) = infCompareAux $ getZipList $ domCompareA <$> x <*> y
    compMin (CDAR.CR x) (CDAR.CR y) = CDAR.CR $ minA <$> x <*> y
    compMax (CDAR.CR x) (CDAR.CR y) = CDAR.CR $ maxA <$> x <*> y


-- powA :: CDAR.Approx -> Int -> CDAR.Approx
-- powA CDAR.Bottom _ = CDAR.Bottom
-- powA (CDAR.Approx m e s) n
--     | even n && am <= e = CDAR.Approx ame ame (n*s-1)
--     | even n && m < 0   = CDAR.Approx (a+b) (b-a) (n*s-1)
--     | otherwise         = CDAR.Approx (a+b) (a-b) (n*s-1)
--     where am = abs m
--           ame = (am + e)^(n :: Int)
--           a = (m + e)^(n :: Int)
--           b = (m - e)^(n :: Int)

instance Powers CDAR.CR
    --the default implementation using x^n is much faster
    --pow x n = CDAR.CR $ (\a l -> CDAR.ok (-100) $ CDAR.limitAndBound l (powA a n)) <$> CDAR.unCR x <*> ZipList resources

instance Boundable CDAR.CR


instance Show CDAR.CR where
    show = CDAR.showA . CDAR.require 80