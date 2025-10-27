{-# LANGUAGE ScopedTypeVariables #-}

module Tests where

import Limit
import CompOrd
import CompReal
import Lang.Expr
import Lang.Parser
import Lang.Interpreter
import Solver.Powers

import Data.Ratio ((%))

--the leibniz series for pi converges slowly, making it useful for benchmarking
piLeibniz :: (Num r, Limit Rational r) => r
piLeibniz = 4 * calabreseSum [1 % (2 * k + 1) | k <- [0..]]

--this serves as a test for the CompReal instance
--if any of its methods are poorly implemented, 'correctionPi piLeibniz' may return false
correctionPi :: (CompReal r) => r -> [Bool]
correctionPi r = map (containsPi . bound r) [0..300]
    where containsPi (l,u) = l <= ratPi && ratPi <= u
          ratPi = 31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679 % 10^100


correctionRational :: (CompReal r) => r -> Rational -> [Bool]
correctionRational r q = map (contains . bound r) [0..]
    where contains (l,u) = l <= q && q <= u


gaussSum :: (Num r) => r
gaussSum = sum $ map fromInteger [1..100] --5050

factorial50 :: (Num r) => r
factorial50 = product $ map fromInteger [1..50] --30414093201713378043612608166064768844377641568960512000000000000

geometricSeriesRat :: (Limit Rational r) => r
geometricSeriesRat = limit $ (!!) $ scanl1 (+) $ iterate (/2) $ 1%2

geometricSeriesCR :: forall r. (Fractional r, Limit r r) => r
geometricSeriesCR = _limit $ (!!) $ map fromRational $ scanl1 (+) $ iterate (/2) $ 1%2
    where _limit = limit :: (Int -> r) -> r

-- [y, v] (nVars=2)
ballBounce :: (SimNum r) => Rational -> r
ballBounce t = fromVal (query (interpret prog) 2 16 100 (fromRational t)) !! 0
    where prog = Seq (Assign 0 (Num 0)) $ Seq (Assign 1 (Num 1)) $ loop
          loop = WhileDo (Term $ BConst True) (Seq arc bounce)
          arc = getFor [(0, Var $ V 1), (1, Num $ -1)] (Just $ Op Mult (Num 2) (Var $ V 1))
          bounce = Assign 1 (Op Mult (Num $ -0.8) (Var $ V 1))