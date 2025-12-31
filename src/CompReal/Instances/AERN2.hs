{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CompReal.Instances.AERN2 (AERN2.CReal) where

import Utils
import qualified SnocList as SL
import CompOrd
import CompReal
import Boundable
import Powers
import Limit

import qualified AERN2.Real as AERN2
import qualified AERN2.MP as AERN2 (endpoints, (+-), mpBallP)
import qualified AERN2.MP.Float as AERN2 (MPFloat)

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
    {-
    This implementation was copied from AERN2.limit and adapted for rationals
    Instead of using cseqPrecisions (which slows everything down when the input list is
    slow to compute), the precisions and indexes are taken from [0..]
    We also use lists instead of a function, which is faster when all terms of the list are used
    -}
    listLimit xs = AERN2.CSequence $ map aux $ zip [0..] (SL.repeatLast $ SL.fromList xs)
        where aux (p,x) = (MTNP.cn $ AERN2.mpBallP p x) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ (MTNP.integer p))

    {-
    Alternatively, we could use an index of the list different from the precision of the dyad
    approximation of the rational (a non diagonal limit). While faster in some cases (more
    specifically, when the list of rationals is slow to compute), on others (when the list is
    fast) it quickly gets bogged down by the excessive precision.
    And of course, because AERN2 is great, if the precision ever goes above 5000000 we get an error
    -}
    -- listLimit xs = AERN2.CSequence $ map aux $ zip3 [0..] AERN2.cseqPrecisions (SL.repeatLast $ SL.fromList xs)
    --     where aux :: (Integer, AERN2.Precision, Rational) -> MTNP.CN AERN2.MPBall
    --           aux (i,p,x) = (MTNP.cn $ AERN2.mpBallP p x) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ i)


--So apparently, AERN2 straight up has a max precision of 5000000 for limits buried in its code (AERN2.cseqPrecisions)
--It's easy to miss, it's buried among all the bloat
--So, you know, don't put too much faith in this instance. At any time it can just.. run out of precision
--It's always cool when your arbitrary precision library runs out of precision
--And it's such an artificial limitation, too. It's literally a hardcoded constant, after which AERN2 simply refuses to compute
--Can you tell I'm angry? I've been digging in this bloated library for days now. Please send help
instance Limit AERN2.CReal AERN2.CReal where
    limit f = AERN2.limit (f . (max 0) . pred)

    listLimit xs = AERN2.CSequence $ SL.foldr aux1 aux2 $ SL.zip (SL.fromList AERN2.cseqPrecisions) elems
        where elems = SL.getIndexesOrLast (map (fromInteger . MTNP.integer) AERN2.cseqPrecisions) (SL.fromList xs)
              aux1 (p, x) = ((x AERN2.? p) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ (MTNP.integer p)) :)
              aux2 (p, x) = MTNP.drop (AERN2.cseqIndexForPrecision p) (AERN2.unCSequence x)

    errorLimit = errorLimitDef

asIntervals :: AERN2.CReal -> [(AERN2.MPFloat,AERN2.MPFloat)]
asIntervals = map (AERN2.endpoints . MTNP.unCN) . AERN2.unCSequence

instance CompOrd AERN2.CReal where
    domCompare = domCompareDef
    infCompare x y = infCompareAux $ zipWith domCompareAux (asIntervals x) (asIntervals y)
    compMin = MTNP.min
    compMax = MTNP.max

instance Powers AERN2.CReal where
    pow _ 0 = 1
    pow x 1 = x
    pow x n = MTNP.pow x n

instance Boundable AERN2.CReal