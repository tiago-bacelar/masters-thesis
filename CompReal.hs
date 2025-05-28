{-# LANGUAGE TypeFamilies, DefaultSignatures #-}

module CompReal where

import Solver.Interval
import Solver.Powers

import qualified Data.CDAR as CDAR
--import qualified AERN2.Real as AERN2
import qualified ERA.CReal as ERA --available in stdlib as Data.Numbers.CReal, but that version doesn't export CR, making it kinda useless
import qualified Data.Number.IReal as IReal
import qualified Data.Number.IReal.IReal as IReal(ir, appr)
import qualified Data.Number.IReal.IntegerInterval as IReal(lowerI, upperI, radI) 

import GHC.Num
import Data.Bits
import Data.List
import Data.Maybe
import Data.Ratio
import Control.Applicative

pow2 :: Int -> Integer
pow2 = shiftL 1

lg2 :: Integer -> Int
lg2 = fromIntegral . GHC.Num.integerLogBase 2

--TODO: partial comparison?
class (Floating r) => CompReal r where
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


    --Limits can be taken from either functions, centerpoint lists or bound lists, and of either Rationals or ComprReals
    --This means 6 limit functions are possible, and translating between them agnostically may incur heavy performance penalties
    --For this reason, the only mandatory implementation in this class is listLimit, since translating to the others has relatively small penalties
    --However, implementing the other functions directly may still be more efficient depending on the specific CompReal type
    --Although implementing listLimit is always recomended, translation functions to listLimit using one of the others are available below

    --limit of a normalized Cauchy sequence (composed with its modulus of convergence)
    --formally, |approx (limit s) n - lim s| <= 2^(-n-1)
    limit :: (Int -> r) -> r
    limit f = listLimit (map f [0..]) id

    limitRatio :: (Int -> Rational) -> r
    limitRatio f = listLimitRatio (map f [0..]) id

    --calculates the limit through its modulus of convergence
    listLimit :: [r] -> (Int -> Int) -> r
    default listLimit :: (Intervalable r) => [r] -> (Int -> Int) -> r
    listLimit s a = boundLimit $ map boundX $ zip [0..] $ composeModulusList a s
        where boundX (i, x) = let r = fromRational (1 % pow2 i+1) in x-r <~> x+r

    listLimitRatio :: [Rational] -> (Int -> Int) -> r
    listLimitRatio s a = boundLimitRatio $ map boundX $ zip [0..] $ composeModulusList a s
        where boundX (i, x) = let r = 1 % pow2 i+1 in (x-r, x+r)

    --calculates the limit from a list of arbitrarily shrinking nested intervals
    --if the list is finite, the last interval must be degenerate (must have a single element)
    --other than that, there are no restrictions on the convergence rate of the intervals
    boundLimit :: (Intervalable r) => [Interval r] -> r

    boundLimitRatio :: [(Rational, Rational)] -> r
    default boundLimitRatio :: (Intervalable r) => [(Rational, Rational)] -> r
    boundLimitRatio = boundLimit . map (\(l,u) -> fromRational l <~> (fromRational u :: r))

    {-# MINIMAL (approx | bound), boundLimit #-}


composeModulusList :: (Int -> Int) -> [a] -> [a]    --TODO: finite lists
composeModulusList a = map head . flip (scanl (flip drop)) dif
    where mcList = map a [0..]  
          dif = head mcList : zipWith (-) (tail mcList) mcList

--penalty from loss of accuracy and diagonal limit
limitFromLimitRatio :: (CompReal r) => (Int -> r) -> r
limitFromLimitRatio f = limitRatio (\i -> approx (f (i + 1)) (i + 1))

--may cause penalties from use of fromRational
limitRatioFromLimit :: (CompReal r) => (Int -> Rational) -> r
limitRatioFromLimit f = limit (fromRational . f)

--time penalty from linear list access (TODO: optimize with btree?)
listLimitFromLimit :: (CompReal r) => [r] -> (Int -> Int) -> r
listLimitFromLimit s a = limit (composeModulusList a s !!)

--listLimitFromListLimitRatio :: (CompReal r) => [r] -> (Int -> Int) -> r
--listLimitFromListLimitRatio s a = listLimitRatio ...  TODO

--may cause penalties from use of fromRational
listLimitRatioFromListLimit :: (CompReal r) => [Rational] -> (Int -> Int) -> r
listLimitRatioFromListLimit = listLimit . map fromRational

--listLimitRatioFromLimitRatio :: (CompReal r) => [Rational] -> (Int -> Int) -> r
--listLimitRatioFromLimitRatio s a = limitRatio ...     TODO


-- (<!) x y p = snd (bound x p) < fst (bound y p)
-- x >! y = y <! x

{-
instance (CompReal r) => CompOrd r where
    mCompare x y p | ux < ly                            = Just LT
                   | lx > uy                            = Just GT
                   | lx == ux && ly == uy && lx == ly   = Just EQ
                   | otherwise                          = Nothing
        where (lx, ux) = bound x p
              (ly, uy) = bound y p

    (<!) x y p = snd (bound x p) < fst (bound y p)
    x >! y = y <! x
-}

--calculates the sum of a series through its modulus of convergence
seriesSum :: (CompReal r) => [r] -> (Int -> Int) -> r
seriesSum = listLimit . scanl1 (+)

seriesSumRatio :: (CompReal r) => [Rational] -> (Int -> Int) -> r
seriesSumRatio = listLimitRatio . scanl1 (+)


instance CompReal CDAR.CR where
    approx r n = toRational $ fromJust $ CDAR.centre $ CDAR.require n r

    --limit = CDAR.limCR --the library's implementation is stupidly bad for expensive sequences
                         --because it looks at the first 70 terms to produce the first Approx
    limitRatio f = CDAR.CR $ ZipList [CDAR.toApprox i (f i) + CDAR.toApprox i 0 | i <- [0..]]

instance Intervalable CDAR.CR where
    type Interval CDAR.CR = CDAR.CR
    x <~> y = CDAR.CR $ ZipList $ zipWith CDAR.unionA (getZipList $ CDAR.unCR x) (getZipList $ CDAR.unCR y)
    lower = undefined
    upper = undefined

instance Powers CDAR.CR



instance CompReal ERA.CReal where
    approx (ERA.CR r) n = r n % pow2 n --TODO: something fishy here... (correctionPi pi)
    --limit f = ERA.CR (\i -> let ERA.CR g = f (i+1) in ERA.round_uk (g (i + 1) % 2))

instance Intervalable ERA.CReal
instance Powers ERA.CReal



--instance CompReal AERN.RealNumber where
--    approx 



--ireal is a bit unique, because it explicitely uses IReals to represent open real intervals
--as well as real numbers. This means an IReal value, which is a function Int->(Integer,Integer),
--isn't guaranteed to converge to a single number at all, which is anoying when typing functions in practice
--This CompReal instance assumes IReals are numbers (all intervals are "thin", that is, have
--a difference of 2) and as such always converge with the expected modulus
instance CompReal IReal.IReal where
    bound r n = let i = IReal.appr r (n + 1); d = pow2 (n + 1) in (IReal.lowerI i % d, IReal.upperI i % d)

    boundLimit rs = IReal.ir (\p -> last (take (p+1) ans) p)
        where ans = aux 0 (zip [0..] rs)
              aux p [(i, r)] = [\p -> IReal.appr r p]
              aux p ((i,r):rs) | IReal.radI a < pow2 (i-p+1) = const a : aux (p+1) ((i,r):rs) --TODO: test properly
                               | otherwise = aux p rs
                            where a = IReal.appr r i

instance Intervalable IReal.IReal where
    type Interval IReal.IReal = IReal.IReal
    (<~>) = (IReal.-+-)
    lower = IReal.lower
    upper = IReal.upper

instance Powers IReal.IReal where
    pow x 0 = 1
    pow x n = IReal.ir f
        where x0 = IReal.appr x 0
              f p = IReal.scale (IReal.pow xp n) (p - n*q)
                where xp = IReal.appr x q
                      q = p + ceiling (logBase 2 (fromIntegral n) :: Double) 
                            + (n-1) * lg2 (IReal.upperI (abs x0)) + n
    --TODO: powers