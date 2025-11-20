{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

import Solver.Powers
import Limit
import CompOrd
import CompReal
import Tests

import Data.Ratio
import Criterion.Types
import Criterion.Main.Options
import Criterion.Main

import qualified Data.CDAR as CDAR
import qualified AERN2.Real as AERN2
import qualified ERA.CReal as ERA
import qualified Data.Number.IReal as IReal


type BenchNum r = (Floating r, Powers r, CompOrd r, Limit Rational r, Limit r r)

--a single run takes a test and a precision and benchmarks taking
--the CompReal outputted from the test up to the given preicison
run :: (CompReal r) => (() -> r) -> Int -> Benchmarkable
run r n = nf (uncurry (approx . r)) ((),n)

--a test groups together runs for every implementation and every precision
test :: String -> [Int] -> (forall r. (BenchNum r) => r) -> Benchmark
test name precisions r = bgroup name [
        bench "Double" $ nf (\() -> (r :: Double)) (),
        iRun "CDAR"  (\() -> (r :: CDAR.CR)),       --we do this stupid lambda thing here to prevent sharing
        iRun "AERN2" (\() -> (r :: AERN2.CReal)),   --in the run function after resolving the polymorphism
        iRun "ERA"   (\() -> (r :: ERA.CReal)),
        --exact-real
        iRun "IReal" (\() -> (r :: IReal.IReal)) ]
    where iRun :: (CompReal r) => String -> (() -> r) -> Benchmark
          iRun iName i = bgroup iName $ map (\p -> bench (show p) $ run i p) precisions


myConfig = defaultConfig {
              timeLimit = 2.0,
              --resamples = 10,
              reportFile = Just "bench/benchmarks/report3.html",
              csvFile = Just "bench/benchmarks/output3.csv"
           }

main :: IO ()
main = defaultMainWith myConfig [
       test "exact_dyadic" [0..10]      $ fromRational $ 35184372088000 % 35184372088832, -- 2^45
       test "dyadic_division" [0..10]   $ 35184372088000 / 35184372088832,
       test "exact_rational" [0..10]    $ fromRational (22 % 7)

       --test "gauss_sum" [0..2]          $ gaussSum
       --test "piLeibniz" [0..6]        $ piLeibniz
       --test "simple_sum" [8,9]       $ piLeibniz + piLeibniz,
       --test "memo_sum" [8,9]         $ let x = piLeibniz in x + x,
       --test "doubling" [8,9]           $ 2 * piLeibniz
       --test "ball_bounce_9" [0..10] $ ballBounce 9
                   ]