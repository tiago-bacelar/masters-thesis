{-# LANGUAGE DefaultSignatures #-}

module CompReal where

import Utils
import Limit
import CompOrd
import Solver.Powers

import qualified ERA.CReal as ERA --available in numbers as Data.Numbers.CReal, but that version doesn't export CR, making it kinda useless
import qualified Data.CDAR as CDAR
--import qualified AERN2.Real as AERN2
import qualified Data.Number.IReal as IReal
import qualified Data.Number.IReal.IReal as IReal(ir, appr)
import qualified Data.Number.IReal.IntegerInterval as IReal(IntegerInterval(..), upperI)

import Data.List
import Data.Maybe
import Data.Ratio
import Control.Applicative

--Same as max, but written without the Ord constraint
maxCR :: (Fractional a) => a -> a -> a
maxCR x y = (x + y + abs (x - y)) / 2

--Same as maximum, but written without the Ord constraint
maximumCR :: (Fractional a) => [a] -> a
maximumCR = foldr1 maxCR



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

--unambiguated aliases of the limit class
unapprox :: (CompReal r) => (Int -> Rational) -> r
unapprox = limit
listLimit :: (CompReal r) => [(Rational, Rational)] -> r
listLimit = errorLimit
realLimit :: (CompReal r) => (Int -> r) -> r
realLimit = limit
realListLimit :: (CompReal r) => [(r, r)] -> r
realListLimit = errorLimit

--a default implementation of realListLimit using realLimit and a CompReal restriction
realListLimitDef :: (CompReal r) => [(r, r)] -> r
realListLimitDef = realLimit . errorLimitAux p
    where p n e = snd (bound e (n+1)) <= 1 % pow2 (n+1)


--TODO: slowly increase prescision if needed?
domCompareDef :: (CompReal r) => r -> r -> Int -> OrderingDomain
domCompareDef x y p | lx == ux && ux == ly && ly == uy  = Top EQ
                    | ux == ly                          = LEQ
                    | lx == uy                          = GEQ
                    | ux < ly                           = Top LT
                    | lx > uy                           = Top GT
                    | otherwise                         = Bottom
    where (lx, ux) = bound x p
          (ly, uy) = bound y p


instance CompReal ERA.CReal where
    approx (ERA.CR r) n = r (n+1) % pow2 (n+1)

instance Limit Rational ERA.CReal where
    limit f = ERA.CR (\i -> ERA.round_uk (f (i+1) * fromInteger (pow2 i)))

instance Limit ERA.CReal ERA.CReal where
    limit f = ERA.CR (\i -> let ERA.CR g = f (i+1) in ERA.round_uk (g (i+1) % 2))
    errorLimit = realListLimitDef

instance CompOrd ERA.CReal where
    domCompare = domCompareDef

instance Powers ERA.CReal



instance CompReal CDAR.CR where
    approx r n = toRational $ fromJust $ CDAR.centre $ CDAR.require n r

instance Limit Rational CDAR.CR where
    limit f = CDAR.CR $ ZipList [CDAR.Approx (round $ (f i)*(toRational $ pow2 i)) 1 (-i) | i <- [0..]]
    
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
    limit = CDAR.limCR
    errorLimit = realListLimitDef

instance CompOrd CDAR.CR where
    domCompare = domCompareDef

instance Powers CDAR.CR



--instance CompReal AERN.RealNumber where
--    approx 



--ireal is a bit unique, because it explicitely uses IReals to represent open real intervals
--as well as real numbers. This means an IReal value, which is a function Int->(Integer,Integer),
--isn't guaranteed to converge to a single number at all, which is anoying when typing functions in practice
--This CompReal instance assumes IReals are numbers (all intervals are "thin", that is, have
--a difference of 2) and as such always converge with the expected modulus
instance CompReal IReal.IReal where
    bound r n = let IReal.I (l,u) = IReal.appr r (n+1); d = pow2 (n+1) in (l % d, u % d)

instance Limit Rational IReal.IReal where
    limit f = IReal.ir (\p -> let r = f (p+1) in fromInteger (pow2 p * (numerator r) `div` denominator r))

instance Limit IReal.IReal IReal.IReal where
    limit f = IReal.ir (\p -> let IReal.I (l,u) = IReal.appr (f (p+1)) (p+1) in IReal.I (l `div` 2, u `div` 2))
    errorLimit = realListLimitDef

instance CompOrd IReal.IReal where
    domCompare = domCompareDef

instance Powers IReal.IReal where
    pow x 0 = 1
    pow x 1 = x
    pow x n = IReal.ir f
        where x0 = IReal.appr x 0
              f p = IReal.scale (IReal.pow xp n) (p - n*q)
                where xp = IReal.appr x q
                      q = p + ceiling (logBase 2 (fromIntegral n) :: Double) 
                            + (n-1) * lg2 (IReal.upperI (abs x0)) + n