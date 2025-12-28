{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CompReal.Instances.AERN2 (AERN2.CReal) where

import Utils
import CompOrd
import CompReal
import Boundable
import Powers
import Limit

import qualified AERN2.Real as AERN2
import qualified AERN2.MP as AERN2 (endpoints, (+-), mpBallP)
import qualified AERN2.MP.Float as AERN2 (MPFloat)
--import qualified AERN2.MP.Float.Conversions as AERN2 (fromRationalCEDU)
--import qualified AERN2.MP.Float.Auxi as AERN2 (ceduCentre, ceduCentreErr)

import qualified MixedTypesNumPrelude as MTNP

--This is a bit scuffed, because when working with accuracy AERN2.(?) performs an optimization of
--skipping some elements of the list before starting a one by one search. And that skipping assumes that
--the values in the list follow cseqPrecisions, which is a standart list of precisions (it starts at 10
--and then grows exponentially). I *think* the way I implemented the limits doesn't break anything. But who
--knows? Worse case scenario, the library just skips perfectly fine terms and takes a long time to compute
--later terms, but either way there is never any risk of giving a wrong value, so I guess it's fine? yay?
--this library is so bloated
instance CompReal AERN2.CReal where
    bound x n = toRational >< toRational $ AERN2.endpoints $ MTNP.unCN $ x AERN2.? AERN2.bits n

instance Limit Rational AERN2.CReal where
    --this implementation was copied from AERN2.limit and adapted for rationals
    --the only big change was using [0..] instead of cseqPrecisions, because skipping terms
    --like that was slowing everything down when the function is slow to compute for big indexes
    --limit f = AERN2.CSequence $ map withPrec [0..]
    --    where withPrec p = (MTNP.cn $ AERN2.mpBallP p $ f $ MTNP.int $ MTNP.integer p) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ (MTNP.integer p))

    --this is basically the same, but on lists instead (better when all terms of the list are used, which they are)
    listLimit xs = AERN2.CSequence [(MTNP.cn $ AERN2.mpBallP p x) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ (MTNP.integer p)) | (p,x) <- zip [0..] xs]
 
--So apparently, AERN2 straight up has a max precision of 5000000 for limits buried in its code
--It's easy to miss, it's buried among all the bloat
--So, you know, don't put too much faith in this instance. At any time it can just.. run out of precision
--It's always cool when your arbitrary precision library runs out of precision
--And it's such an artificial limitation, too. It's literally a hardcoded constant, after which AERN2 simply refuses to compute
--Can you tell I'm angry? I've been digging in this bloated library for days now. Please send help
instance Limit AERN2.CReal AERN2.CReal where
    limit f = AERN2.limit (f . (max 0) . pred)
    errorLimit = errorLimitDef --TODO?

asIntervals :: AERN2.CReal -> [(AERN2.MPFloat,AERN2.MPFloat)]
asIntervals = map (AERN2.endpoints . MTNP.unCN) . AERN2.unCSequence

instance CompOrd AERN2.CReal where
    domCompare = domCompareDef
    infCompare x y = infCompareAux $ zipWith domCompareAux (asIntervals x) (asIntervals y)
    compMin = MTNP.min
    compMax = MTNP.max

instance Powers AERN2.CReal where
    pow = MTNP.pow

instance Boundable AERN2.CReal