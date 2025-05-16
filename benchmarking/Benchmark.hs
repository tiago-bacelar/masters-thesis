{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

--module Benchmarking where

import CompReal
import Tests

import Data.Ratio
import Criterion.Types
import Criterion.Main.Options
import Criterion.Main

import qualified Data.CDAR as CDAR
import qualified AERN2.Real as AERN2
import qualified ERA.CReal as ERA


--a single run takes a test and a precision and benchmarks taking
--the CompReal outputted from the test up to the given preicison
run :: (CompReal r) => (() -> r) -> Int -> Benchmarkable
run r n = nf (uncurry (approximate . r)) ((),n)

--a test groups together runs for every implementation and every precision
test :: String -> [Int] -> (forall r. (CompReal r) => r) -> Benchmark
test name precisions r = bgroup name [
        iRun "CDAR"  (\() -> (r :: CDAR.CR)),       --we do this stupid lambda thing here to prevent sharing
        iRun "AERN2" (\() -> (r :: AERN2.CReal)),   --in the run function after resolving the polymorphism
        iRun "ERA"   (\() -> (r :: ERA.CReal)) ]
    where iRun :: (CompReal r) => String -> (() -> r) -> Benchmark
          iRun iName i = bgroup iName $ map (\p -> bench (show p) $ run i p) precisions


myConfig = defaultConfig {
              timeLimit = 5.0,
              --resamples = 10,
              reportFile = Just "benchmarks/report1.html",
              csvFile = Just "benchmarks/output1.csv"
           }

--WHEN RUNNING, COMPILE WITH: ghc -O --make Benchmarking
main = defaultMainWith myConfig [
       --test "exact_dyadic"      $ fromRational $ 35184372088000 % 35184372088832, -- 2^45
       --test "dyadic_division"   $ 35184372088000 / 35184372088832,
       --test "exact_rational"    $ fromRational (22 % 7),

       test "piLeibniz" [8,9]        $ piLeibniz,
       test "simple_sum" [8,9]       $ piLeibniz + piLeibniz,
       test "memo_sum" [8,9]         $ let x = piLeibniz in x + x,
       test "double" [8,9]           $ 2 * piLeibniz
                   ]