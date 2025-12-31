{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Tests (module Tests) where

import Limit
import CompReal
import Lang.Expr
import Lang.Hybrid
import Lang.Parser
import Lang.Interpreter

import Data.Ratio ((%))

--the leibniz series for pi converges slowly, making it useful for benchmarking
piLeibniz :: (Limit Rational r) => r
piLeibniz = calabreseSum [4 % (2 * k + 1) :: Rational | k <- [0..]]


--given the actual value of a CompReal (as a Rational), tests the correction of all its approximations
correction :: (CompReal r) => Rational -> r -> [Bool]
correction ans r = map (contains . bound r) [0..]
    where contains (l,u) = l <= ans && ans <= u

--given the limit of a sequence, returns a normalized sequence where term n is an accuracy n approximation
--if the sequence is finite, the limit is appended to the sequence as its last element
normalize :: Rational -> [Rational] -> [Rational]
normalize l = aux (1%2)
    where aux e (x:xs) | l - x <= e = x : aux (e/2) (x:xs)
                       | otherwise  = aux e xs
          aux _ [] = [l]

--this serves as a test for the CompReal instance
--if any of its methods are poorly implemented, 'correctionPi pi'
--or 'correctionPi piLeibniz' may return false
correctionPi :: (CompReal r) => r -> [Bool]
correctionPi = take 300 . correction ratPi
    where ratPi = 31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679 % 10^(100 :: Integer)

--if any of CompReals methods are poorly implemented, 'correctionSqrt2 (2 ** 0.5)' may return false
correctionSqrt2 :: (CompReal r) => r -> [Bool]
correctionSqrt2 = take 300 . correction ratSqrt2
    where ratSqrt2 = 14142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727 % 10^(100 :: Integer)


gaussSum :: (Num r) => r
gaussSum = sum $ map fromInteger [1..100] --5050

factorial50 :: (Num r) => r
factorial50 = product $ map fromInteger [1..50] --30414093201713378043612608166064768844377641568960512000000000000

--the sum 1/2 + 1/4 + 1/8 + .... = 1
geometricSeriesDyadRat :: (Limit Rational r) => r
geometricSeriesDyadRat = Limit.listLimit $ scanl1 (+) $ iterate (/2) $ (1%2 :: Rational)

--the sum 1/3 + 1/9 + 1/27 + ... = 1/2
geometricSeriesRat :: (Limit Rational r) => r
geometricSeriesRat = Limit.listLimit $ normalize (1%2) $ scanl1 (+) $ iterate (/3) $ (1%3 :: Rational)

--the sum 1/2 + 1/4 + 1/8 + .... = 1
geometricSeriesDyadCR :: forall r. (Fractional r, Limit r r) => r
geometricSeriesDyadCR = _listLimit $ map fromRational $ scanl1 (+) $ iterate (/2) $ 1%2
    where _listLimit = Limit.listLimit :: [r] -> r

--the sum 1/3 + 1/9 + 1/27 + ... = 1/2
geometricSeriesCR :: forall r. (Fractional r, Limit r r) => r
geometricSeriesCR = _listLimit $ map fromRational $ normalize (1%2) $ scanl1 (+) $ iterate (/3) $ 1%3
    where _listLimit = Limit.listLimit :: [r] -> r


-- [y, v] (nVars=2)
ballBounce :: (SimNum r) => Rational -> r
ballBounce t = runQuery (query (interpret prog) 2 (Just 16) (Just 100) (fromRational t)) (Just 16) !! 0 !! 0
    where prog = Seq (Assign 0 (Num 0)) $ Seq (Assign 1 (Num 1)) $ loop
          loop = WhileDo (Term $ BConst True) (Seq arc bounce)
          arc = getFor [(0, Var $ V 1), (1, Num $ -1)] (Just $ Op Mult (Num 2) (Var $ V 1))
          bounce = Assign 1 (Op Mult (Num $ -0.8) (Var $ V 1))