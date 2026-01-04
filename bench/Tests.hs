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

--for testing in ghci
timeApproxs :: (CompReal r) => r -> [Int]
correction ans r = map (\i -> approx r `seq` i) [0..]



--the leibniz series for pi converges slowly, making it useful for benchmarking
piLeibniz :: (Limit Rational r) => r
piLeibniz = calabreseSum [4 % (2 * k + 1) :: Rational | k <- [0..]]

--5050
gaussSum :: (Num r) => r
gaussSum = sum $ map fromInteger [1..100] 

--30414093201713378043612608166064768844377641568960512000000000000
factorial50 :: (Num r) => r
factorial50 = product $ map fromInteger [1..50] 



{-
Runs the following Jaguar program and evaluates it at t=10:

y := 0; v := 1;
while true do { y'=v,v'=-1 for 2 * v; v := -0.8 * v }
-}
ballBounce :: (SimNum r) => Rational -> r
ballBounce t = (!! 0) $ (!! 0) $ (`runQueryJust` 16) $ query (interpret prog) 2 (Just 16) (Just 100) (fromRational t)
    where prog = Seq (Assign 0 (Num 0)) $ Seq (Assign 1 (Num 1)) $ loop
          loop = WhileDo (Term $ BConst True) (Seq arc bounce)
          arc = getFor [(0, Var $ V 1), (1, Num $ -1)] (Just $ Op Mult (Num 2) (Var $ V 1))
          bounce = Assign 1 (Op Mult (Num $ -0.8) (Var $ V 1))