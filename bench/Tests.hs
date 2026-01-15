{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Tests (module Tests) where

{-
This module contains benchmarking tests. Since the code they are testing
is usually optimized to share some values (e.g. Interpreter.query shares state
between queries at different times and query accuracy), many of these tests
require -fno-full-laziness to generate correct benchmarking results
-}

import Limit
import Powers
import Boundable
import CompOrd
import CompReal hiding (limit, listLimit)
import Lang.Expr
import Lang.Hybrid
import Lang.Parser
import Lang.Interpreter
import Solver.Coef
import Solver.Poly
import Solver.Solver

import Data.Ratio ((%))

--for testing in ghci
timeApproxs :: (CompReal r) => r -> [Int]
timeApproxs x = map (\i -> approx x `seq` i) [0..]



--the leibniz series for pi converges slowly and consistently, making it useful for benchmarking
--UPDATE: apparently, when running stack bench the list of rationals is memoized smh
--        (even though ghci doesn't memoize it), making piLeibniz almost instantaneous
piLeibniz :: (Limit Rational r) => r
piLeibniz = calabreseSum [(4 % (2 * k + 1) :: Rational) | k <- [0..]]

--5050
gaussSum :: (Num r) => r
gaussSum = sum $ map fromInteger [1..100] 

--30414093201713378043612608166064768844377641568960512000000000000
factorial50 :: (Num r) => r
factorial50 = product $ map fromInteger [1..50] 



--given the limit of a sequence, returns a normalized sequence where term n is an accuracy n approximation
--if the sequence is finite, the limit is appended to the sequence as its last element
normalize :: Rational -> [Rational] -> [Rational]
normalize l = aux (1%2)
    where aux e (x:xs) | l - x <= e = x : aux (e/2) (x:xs)
                       | otherwise  = aux e xs
          aux _ [] = [l]

--the sum 1/2 + 1/4 + 1/8 + .... = 1
geometricSeriesDyad :: [Rational]
geometricSeriesDyad = scanl1 (+) $ iterate (/2) (1%2)

--the sum 1/3 + 1/9 + 1/27 + ... = 1/2
geometricSeries :: [Rational]
geometricSeries = normalize (1%2) $ scanl1 (+) $ iterate (/3) (1%3)

finiteList :: [Rational]
finiteList = [-0.2, 0.045, 1%6, 2%7]


--1
geometricSeriesDyadRat :: (Limit Rational r) => r
geometricSeriesDyadRat = listLimit geometricSeriesDyad

--1
geometricSeriesDyadCR :: forall r. (Fractional r, Limit r r) => r
geometricSeriesDyadCR = _listLimit $ fromRational <$> geometricSeriesDyad
    where _listLimit = listLimit :: [r] -> r

--0.5
geometricSeriesRat :: (Limit Rational r) => r
geometricSeriesRat = listLimit geometricSeries

--0.5
geometricSeriesCR :: forall r. (Fractional r, Limit r r) => r
geometricSeriesCR = _listLimit $ fromRational <$> geometricSeries
    where _listLimit = listLimit :: [r] -> r

--2/7
finiteListRat :: (Limit Rational r) => r
finiteListRat = listLimit finiteList

--2/7
finiteListCR :: forall r. (Fractional r, Limit r r) => r
finiteListCR = _listLimit $ fromRational <$> finiteList
    where _listLimit = listLimit :: [r] -> r


{-
Calculates the solution of following ODE (which is x(t)=at) at t:
x' = a, x0 = 0
The rational version takes advantage of Coef's rational representation
-}
linearODE :: (Fractional r, Powers r, CompOrd r, Limit r r, Boundable r) => Rational -> Rational -> r
linearODE a t = solvePoly [(0, constPoly $ numCoef $ fromRational a)] (fromRational t) !! 0
linearODERat :: (Fractional r, Powers r, CompOrd r, Limit r r, Boundable r) => Rational -> Rational -> r
linearODERat a t = solvePoly [(0, constPoly $ fromRational a)] (fromRational t) !! 0

{-
Calculates the solution of following ODE (which is x(t)=(a/2)t^2) at t:
x' = v, v' = a, x0 = v0 = 0
The rational version takes advantage of Coef's rational representation
-}
squareODE :: (Fractional r, Powers r, CompOrd r, Limit r r, Boundable r) => Rational -> Rational -> r
squareODE a t = solvePoly [(0, varPoly 1), (0, constPoly $ numCoef $ fromRational a)] (fromRational t) !! 0
squareODERat :: (Fractional r, Powers r, CompOrd r, Limit r r, Boundable r) => Rational -> Rational -> r
squareODERat a t = solvePoly [(0, varPoly 1), (0, constPoly $ fromRational a)] (fromRational t) !! 0

{-
Calculates the solution of following ODE (which is x(t)=e^t) at t:
x' = x, x0 = 1
-}
expODE :: (Fractional r, Powers r, CompOrd r, Limit r r, Boundable r) => Rational -> r
expODE t = solvePoly [(1, varPoly 0)] (fromRational t) !! 0



{-
Runs the following Jaguar program and evaluates it at t:

y := 0; v := 1;
while true do { y'=v,v'=-1 for 2 * v; v := -0.8 * v }

With a comparison and query accuracy of 8 and 9
respectively, the program may not be accurate for t>9.9
-}
ballBounce :: (SimNum r) => Rational -> r
ballBounce = \t -> (!! 0) $ (!! 0) $ (`runQueryJust` 8) $ query (interpret prog) 2 (Just 9) Nothing (fromRational t)
    where prog = Seq (Assign 0 (Num 0)) $ Seq (Assign 1 (Num 1)) $ loop
          loop = WhileDo (Term $ BConst True) (Seq arc bounce)
          arc = getFor [(0, Var $ V 1), (1, Num $ -1)] (Just $ Op Mult (Num 2) (Var $ V 1))
          bounce = Assign 1 (Op Mult (Num $ -0.8) (Var $ V 1))

{-
Runs the following Jaguar program and evaluates it at t:

p :=0;  v :=0; 
pl:=50; vl:=10;
while true do {
  if (v-8)^2 + 4*(p-pl+v-9) < 0
  then p'=v, v'= 2, pl'=vl, vl'=0 for 1
  else p'=v, v'=-2, pl'=vl, vl'=0 for 1
}
-}
cruiseControl :: (SimNum r) => Rational -> r
cruiseControl = \t -> (!! 0) $ (!! 0) $ (`runQueryJust` 1) $ query (interpret prog) (length vars) (Just 1) Nothing (fromRational t)
    where (vars,prog) = case parseJaguar code of
                            Failed err -> error ("Parse error: " ++ show err)
                            Ok p -> p
          code = "p:=0;v:=0;pl:=50;vl:=10;while true do{if (v-8)^2 + 4*(p-pl+v-9) < 0 then p'=v,v'= 2,pl'=vl,vl'=0 for 1 else p'=v,v'=-2,pl'=vl,vl'=0 for 1}"
