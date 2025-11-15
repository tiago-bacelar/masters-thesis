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

import Data.Bits
import Data.List
import Data.Maybe
import Data.Ratio
import Control.Applicative


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


class Boundable r where
    bounds :: r -> (Integer, Integer)

    default bounds :: (RealFrac r) => r -> (Integer, Integer)
    bounds x = (floor x, ceiling x)

instance Boundable Double

lowerBound :: (Boundable r) => r -> Integer
lowerBound = fst . bounds

upperBound :: (Boundable r) => r -> Integer
upperBound = snd . bounds

--a default implementation of Boundable's bounds using a CompReal restriction
boundsDef :: (CompReal r) => r -> (Integer, Integer)
boundsDef = (floor >< ceiling) . flip bound 0

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
    limit f = ERA.CR (\i -> ERA.round_uk (f i * fromInteger (pow2 i)))

instance Limit ERA.CReal ERA.CReal where
    limit f = ERA.CR (\i -> let ERA.CR g = f (i+1) in ERA.round_uk (g (i+1) % 2))
    errorLimit = realListLimitDef

instance CompOrd ERA.CReal where
    domCompare = domCompareDef
    compMax = max
    compMin = min

instance Boundable ERA.CReal where
    bounds = boundsDef

instance Powers ERA.CReal where
    pow x 0 = 1
    pow x 1 = x
    pow (ERA.CR x') n = ERA.CR f
        where x0 = x' 0
              scale x n | n >= 0    = shift x n
                        | otherwise = shift (x + bit (-n-1)) n
              f p = scale (xp ^ n) (p - n*q)
                where xp = x' q
                      q = p + ceiling (logBase 2 (fromIntegral n) :: Double) 
                            + (n-1) * lg2 (abs x0 + 1) + n



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
    errorLimit = realListLimitDef --TODO??

instance CompOrd CDAR.CR where
    domCompare = domCompareDef
    infCompare x y = head $ catMaybes $ getZipList $ CDAR.compareCR x y
    compMax = compMaxDef --TODO?
    compMin = compMinDef --TODO?

instance Boundable CDAR.CR where
    bounds = boundsDef

powA :: Int -> CDAR.Approx -> CDAR.Approx
powA n CDAR.Bottom = CDAR.Bottom
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
    pow x 0 = 1
    pow x 1 = x
    pow x n = CDAR.CR $ fmap (powA n) $ CDAR.unCR x
    --powers x = 1 : x : (map (CDAR.CR . ZipList) $ transpose $ map powersA $ getZipList $ CDAR.unCR x)
    --this implementation of powers is only (slightly) more efficient when later terms require less precision than earlier terms



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
    compMax = max
    compMin = min

instance Boundable IReal.IReal where
    bounds = boundsDef

instance Powers IReal.IReal where
    pow x 0 = 1
    pow x 1 = x
    pow x n = IReal.ir f
        where x0 = IReal.appr x 0
              f p = IReal.scale (IReal.pow xp n) (p - n*q)
                where xp = IReal.appr x q
                      q = p + ceiling (logBase 2 (fromIntegral n) :: Double) 
                            + (n-1) * lg2 (IReal.upperI (abs x0)) + n