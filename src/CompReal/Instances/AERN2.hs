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
import qualified AERN2.MP as AERN2 (endpoints, (+-), mpBallP, getAccuracy)
import qualified AERN2.MP.Float as AERN2 (MPFloat(..))

import qualified MixedTypesNumPrelude as MTNP

-- This is a bit scuffed, because when working with accuracy AERN2.(?) performs an optimization
-- of skipping some elements of the list before starting a one by one search. And that skipping
-- assumes that the values in the list follow cseqPrecisions, which is a standart list of
-- precisions (it starts at 10 and then grows exponentially). Therefore, depending on how limits
-- are impemented, extracting approximations may force many more terms of the limited list than
-- necessary, tanking performance when the list is slow to compute
instance CompReal AERN2.CReal where
    bound x n = toRational >< toRational $ AERN2.endpoints $ MTNP.unCN $ x AERN2.? AERN2.bits (n+1)

instance Limit Rational AERN2.CReal where
    -- This implementation was copied from AERN2.limit and adapted for lists and rationals
    listLimit xs = AERN2.CSequence $ map aux xs'
        where xs' = getIndexes (map (fromInteger . MTNP.integer) AERN2.cseqPrecisions) $ zip [1..] $ SL.repeatLast $ SL.fromList xs
              aux (p,x) = (MTNP.cn $ AERN2.mpBallP (max 2 p) x) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ (MTNP.integer p))

    {-
    Instead of using cseqPrecisions (which slows everything down when the input list is slow
    to compute), we could also use all elements of xs
    -}
    -- listLimit xs = AERN2.CSequence $ map aux $ zip [1..] (SL.repeatLast $ SL.fromList xs)
    --     where aux (p,x) = (MTNP.cn $ AERN2.mpBallP (max 2 p) x) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ (MTNP.integer p))

    {-
    Alternatively, we could use an index of the list different from the precision of the dyad
    approximation of the rational (a non diagonal limit). While faster in some cases (more
    specifically, when the list of rationals is slow to compute), on others (when the list is
    fast) it quickly gets bogged down by the excessive precision.
    And of course, if the precision ever goes above 5000000 we get an error (read comment below)
    -}
    -- listLimit xs = AERN2.CSequence $ map aux $ zip3 [0..] AERN2.cseqPrecisions (SL.repeatLast $ SL.fromList xs)
    --     where aux :: (Integer, AERN2.Precision, Rational) -> MTNP.CN AERN2.MPBall
    --           aux (i,p,x) = (MTNP.cn $ AERN2.mpBallP p x) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ i)


--So apparently, AERN2 straight up has a max precision of 5000000 for limits buried in its code (AERN2.cseqPrecisions)
--So, you know, don't put too much faith in this instance. At any time it can just.. run out of precision
--It's always cool when your arbitrary precision library runs out of precision
--And it's such an artificial limitation, too. It's literally a hardcoded constant, after which AERN2 simply refuses to compute
instance Limit AERN2.CReal AERN2.CReal where
    limit f = AERN2.limit (f . (max 0) . pred)

    listLimit xs = AERN2.CSequence $ SL.foldr aux1 aux2 xs'
        where xs' = SL.getIndexesOrLast (map (fromInteger . MTNP.integer) AERN2.cseqPrecisions) $ SL.fromList $ zip [1..] xs
              aux1 (p, x) = ((x AERN2.? p) AERN2.+- ((0.5 :: AERN2.MPFloat) ^ (MTNP.integer p)) :)
              aux2 (p, x) = MTNP.drop (AERN2.cseqIndexForPrecision p) (AERN2.unCSequence x)

    errorLimit = errorLimitDef

asIntervals :: AERN2.CReal -> [(AERN2.MPFloat,AERN2.MPFloat)]
asIntervals = map (AERN2.endpoints . MTNP.unCN) . AERN2.unCSequence

asIntervalsUntil :: Int -> AERN2.CReal -> SL.SnocList (AERN2.MPFloat,AERN2.MPFloat)
asIntervalsUntil n = fmap (AERN2.endpoints . MTNP.unCN) . SL.takeUntil ((>= AERN2.bits (n+2)) . AERN2.getAccuracy) . AERN2.unCSequence

instance CompOrd AERN2.CReal where
    domCompare x y n = SL.findOrLast isTop $ SL.zipOrLastWith domCompareAux (asIntervalsUntil n x) (asIntervalsUntil n y)
    infCompare x y = infCompareAux $ zipWith domCompareAux (asIntervals x) (asIntervals y)
    compMin = MTNP.min
    compMax = MTNP.max

instance Powers AERN2.CReal where
    pow _ 0 = 1
    pow x 1 = x
    pow x n = MTNP.pow x n

instance Boundable AERN2.CReal