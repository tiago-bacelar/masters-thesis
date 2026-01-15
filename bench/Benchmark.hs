{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

import Utils
import Limit
import Powers
import Boundable
import CompOrd
import CompReal
import Tests
import Group
import PlotBench

import Data.Ratio ((%))
import Data.List (foldl1', intersperse)
import GHC.Data.Maybe (firstJust)
import Control.Exception (AsyncException(..), try, catch, throwIO)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Text.Regex.TDFA ((=~))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isAlreadyExistsError)
import System.Directory (createDirectory, createDirectoryIfMissing, removeFile)
import Criterion.Types
import Criterion.Main.Options
import Criterion.Main

import qualified CompReal.Instances.CDAR as CDAR
import qualified CompReal.Instances.AERN2 as AERN2
import qualified CompReal.Instances.ERA as ERA
import qualified CompReal.Instances.ExactReal as ExactReal
import qualified CompReal.Instances.IReal as IReal


type BenchNum r = (Floating r, Powers r, Boundable r, CompOrd r, Limit Rational r, Limit r r)

--a single run takes a test and an accuracy and benchmarks taking
--the CompReal outputted from the test up to the given preicison
run :: (CompReal r) => (a -> r) -> a -> Int -> Benchmark
run f a n = bench (show n) $ nf (uncurry (approx . f)) (a,n)

--a group of runs of the same test across all given accuracies
nRuns :: (CompReal r) => String -> (a -> r) -> a -> [Int] -> Benchmark
nRuns i f a = bgroup i . map (run f a)

--a group of runs of the same test across all CompReal implementations
iRuns :: forall a. (forall r. (BenchNum r) => a -> r) -> a -> [Int] -> [Benchmark]
iRuns f a ns = [
      bench "Double" $ nf (f :: a -> Double) a
    , nRuns "CDAR"      (f :: a -> CDAR.CR) a ns
    , nRuns "AERN2"     (f :: a -> AERN2.CReal) a ns
    , nRuns "ERA"       (f :: a -> ERA.CReal) a ns
    , nRuns "ExactReal" (f :: a -> ExactReal.AnyCReal) a ns
    , nRuns "IReal"     (f :: a -> IReal.IReal) a ns
    ]

--a test groups together runs for every implementation and every accuracy
test :: String -> [Int] -> (forall r. (BenchNum r) => r) -> Benchmark
test name ns r = bgroup name $ iRuns (\() -> r) () ns

--testParam is the same as test but accepts another parameter
testParam :: (Show a) => String -> [a] -> [Int] -> (forall r. (BenchNum r) => a -> r) -> Benchmark
testParam name params ns f = bgroup name $ map (\a -> bgroup (show a) $ iRuns f a ns) params



jsonPath = (++ "benchmarks.json")
csvPath = (++ "benchmarks.csv")
reportPath = (++ "report.html")

myConfig :: String -> Config
myConfig outputPath = defaultConfig {
        timeLimit = 1.5, --MINIMUM time spent on each bench; default is 5s, min appears to be 1.5s
        resamples = 10,  --MINIMUM number of reruns of a bench
        --regressions = [(["iters"],"cycles")], --ADDITIONAL regressions to perform besides time/iters, which is always performed
        jsonFile = Just $ jsonPath outputPath, --full results
        csvFile = Just $ csvPath outputPath, --summary of results
        --reportFile = Just reportFP,  --pretty sure this is raising an error when there are too many tests
        verbosity = Verbose
    }


benchmarks :: [Benchmark]
benchmarks = [
       -- test "fromRational" [0,10..500]       $ fromRational (22 % 7)

    -- , test "sum" [0,5..400]                 $ fromRational (22 % 7) + fromRational (22 % 7)
    -- , test "memo_sum" [0,5..400]            $ let x = fromRational (22 % 7) in x + x
    -- , test "list_sum" [0,5..400]            $ sum [fromRational (22 % 7) | _ <- [1..100]]
    -- , test "list_memo_sum" [0,5..400]       $ sum $ replicate 100 $ fromRational (22 % 7)
    -- , test "tree_sum" [0,5..400]            $ foldTree1 (+) [fromRational (22 % 7) | _ <- [1..100]]
    -- , test "tree_memo_sum" [0,5..400]       $ foldTree1 (+) $ replicate 100 $ fromRational (22 % 7)

    -- , test "product" [0,5..400]             $ fromRational (22 % 7) * fromRational (22 % 7)
    -- , test "memo_product" [0,5..400]        $ let x = fromRational (22 % 7) in x * x
    -- , test "list_product" [0,5..400]        $ product [fromRational (22 % 7) | _ <- [1..100]]
    -- , test "list_memo_product" [0,5..400]   $ product $ replicate 100 $ fromRational (22 % 7)
    -- , test "tree_product" [0,5..400]        $ foldTree1 (*) [fromRational (22 % 7) | _ <- [1..100]]
    -- , test "tree_memo_product" [0,5..400]   $ foldTree1 (*) $ replicate 100 $ fromRational (22 % 7)

    -- , test "power" [0,5..400]               $ fromRational (22 % 7) ^ 100 --equivalent to tree_product
    -- , test "Power.pow" [0,5..400]           $ pow (fromRational (22 % 7)) 100

    -- , test "pi" [0,5..400]                  $ pi
    -- , test "e" [0,5..400]                   $ exp 1
    -- , test "exp 100" [0,5..400]             $ exp 100

    -- , test "geometricSeriesDyadRat" [0,5..400]  $ geometricSeriesDyadRat
    -- , test "geometricSeriesDyadCR" [0,5..400]   $ geometricSeriesDyadCR
    -- , test "geometricSeriesRat" [0,5..400]      $ geometricSeriesRat
    -- , test "geometricSeriesCR" [0,5..400]       $ geometricSeriesCR
    -- , test "finiteListRat" [0,5..400]           $ finiteListRat
    -- , test "finiteListCR" [0,5..400]            $ finiteListCR


    -- , test "list_sum_dif" [0,5..400]                    $ sum [fromRational (i % 7) | i <- [1..100]]
    -- , test "tree_sum_dif" [0,5..400]                    $ foldTree1 (+) [fromRational (i % 7) | i <- [1..100]]

    -- , test "list_product_dif" [0,5..400]                $ product [fromRational (i % 7) | i <- [1..100]]
    -- , test "tree_product_dif" [0,5..400]                $ foldTree1 (*) [fromRational (i % 7) | i <- [1..100]]

    -- , test "geometricSeriesCR_list_sum" [0,5..400]      $ sum [geometricSeriesCR | _ <- [1..100]]
    -- , test "geometricSeriesCR_list_memo_sum" [0,5..400] $ sum $ replicate 100 $ geometricSeriesCR
    -- , test "geometricSeriesCR_tree_sum" [0,5..400]      $ foldTree1 (+) [geometricSeriesCR | _ <- [1..100]]
    -- , test "geometricSeriesCR_tree_memo_sum" [0,5..400] $ foldTree1 (+) $ replicate 100 $ geometricSeriesCR

    -- , test "geometricSeriesCR_list_product" [0,5..400]      $ product [geometricSeriesCR | _ <- [1..100]]
    -- , test "geometricSeriesCR_list_memo_product" [0,5..400] $ product $ replicate 100 $ geometricSeriesCR
    -- , test "geometricSeriesCR_tree_product" [0,5..400]      $ foldTree1 (*) [geometricSeriesCR | _ <- [1..100]]
    -- , test "geometricSeriesCR_tree_memo_product" [0,5..400] $ foldTree1 (*) $ replicate 100 $ geometricSeriesCR

      testParam "list_sum" [1000,2000..20000] [0,100..400]  $ \m -> foldl1' (+) [fromRational (i % 7) | i <- [1..m]]
    , testParam "tree_sum" [1000,2000..20000] [0,100..400]  $ \m -> foldTree1 (+) [fromRational (i % 7) | i <- [1..m]]

    -- , testParam "idODE"                      [0..15] [0,20..200] $ linearODE 1 . toRat        --30 min each
    -- , testParam "idODE_rat"                  [0..15] [0,20..200] $ linearODERat 1 . toRat
    -- , testParam "doubleODE"                  [0..15] [0,20..200] $ linearODE 2 . toRat
    -- , testParam "doubleODE_rat"              [0..15] [0,20..200] $ linearODERat 2 . toRat
    -- , testParam "tripleODE"                  [0..15] [0,20..200] $ linearODE 3 . toRat
    -- , testParam "tripleODE_rat"              [0..15] [0,20..200] $ linearODERat 3 . toRat
    -- , testParam "squareODE1"                 [0..15] [0,20..200] $ squareODE 2 . toRat
    -- , testParam "squareODE1_rat"             [0..15] [0,20..200] $ squareODERat 2 . toRat
    -- , testParam "squareODE2"                 [0..15] [0,20..200] $ squareODE 4 . toRat
    -- , testParam "squareODE2_rat"             [0..15] [0,20..200] $ squareODERat 4 . toRat
    -- , testParam "squareODE3"                 [0..15] [0,20..200] $ squareODE 6 . toRat
    -- , testParam "squareODE3_rat"             [0..15] [0,20..200] $ squareODERat 6 . toRat

    -- , testParam "ball_bounce" ([2..8]++[9,9.1..9.9]) [0,20..200] $ ballBounce . toRat         --at least 13h, probably way more
    -- , testParam "cruise_control"             [0..50] [0,20..200] $ cruiseControl . toRat      --3h

    --ODEs lineares com o param sendo o coef??


    --TODO: all memo tests again (now with full-laziness)
    --TODO: limits, nested limits
    --TODO: consecutive odes


    --TODO: fromRational (to test lost accuracy of approximations)
    --      basic operations (sum, mult, div, nat pow, pow, sqrt) (careful: take into account the fromRational)
    --      memoization (using sum as an example ig)
    --      limits (careful: take into account the time taken to generate the terms)
    --      ode solver (constant, linear, polynomial, exponential/trig/recursive)
    --      jaguar (exp precision, ACC pilot)

    --TODO: write script to graph benchmark times of ode as function of accuracy and time

    --test "gauss_sum" [0..2]          $ gaussSum
    --test "piLeibniz" [0..6]        $ piLeibniz
    --test "simple_sum" [8,9]       $ piLeibniz + piLeibniz
    --test "memo_sum" [8,9]         $ let x = piLeibniz in x + x
    --test "doubling" [8,9]           $ 2 * piLeibniz
    --test "ball_bounce_9" [0..10] $ ballBounce 9
    ]


type Test a = OptGroup Rational (Group String (OptGroup Int a))
type Tests a = Group String (Test a)

splitTestName :: (String, a) -> (String, (Maybe Rational, (String, (Maybe Int, a))))
splitTestName (s,a) = (gs !! 0, (readRat (gs !!? 2) (gs !!? 4), (gs !! 5, (read <$> (gs !!? 7), a))))
    where (_,_,_,gs) = s =~ "^([a-zA-Z0-9_\\-]+)(/(-?[0-9]+(\\.[0-9]+)?)|(-?[0-9]+%[0-9]+))?/([a-zA-Z0-9_\\-]+)(/([0-9]+))?$" :: (String,String,String,[String])
          xs !!? i = let m = xs !! i in if null m then Nothing else Just m
          readRat float frac = firstJust (readDecimal <$> float) (read <$> frac)

fromRuns :: [(String, a)] -> Tests a
fromRuns = fmap (fmap (fmap (fmap unSingleton . optGroupSplit) . groupSplit) . optGroupSplit) . groupSplit . map splitTestName
    where unSingleton [x] = x
          unSingleton _   = error "fromRuns: Duplicated runs found on the run list"


-- reading the regression from the json could provide better results
-- readFromJSON :: String -> IO [Test Double]
-- readFromJSON = do
--     contents <- readFile (jsonPath outputPath)
--     let runs = ...
--     return $ fromRuns runs

readFromCSV :: String -> IO (Tests Double)
readFromCSV outputPath = do
    contents <- readFile (csvPath outputPath)
    let csv = drop 1 $ strSplit ',' <$> lines contents --parse csv, skip header
    let runs = (\l -> (l !! 0, 1000 * read (l !! 1))) <$> csv
    return $ fromRuns runs

--plot time vs param for each accuracy
fixedAccuracy :: Group Rational (Group String (OptGroup Int Double)) -> [(String, [(String, [(Rational, Double)])])]
fixedAccuracy = unG . pmap (("n=" ++) . show) . fmap unG . swapGroup . fmap (fmap unG . swapGroup) . swapGroup . fmap optGroups

--plot time vs param for each implementation
fixedImplementation :: Group Rational (Group String (OptGroup Int Double)) -> [(String, [(Int, [(Rational, Double)])])]
fixedImplementation = unG . fmap (maybeGrouped 0 . fmap unG . swapOptGroup) . swapGroup

generateTestPlots :: String -> String -> Test Double -> IO ()
generateTestPlots outputPath name (Single g) = plotAccuracy outputPath name $ unG $ fmap unOptG g
generateTestPlots outputPath name (Grouped g) = genDir >> sequence_ multiIPlots >> sequence_ multiAccPlots
    where dir = outputPath ++ name ++ "/"
          genDir = createDirectoryIfMissing False dir
          multiIPlots = uncurry (plotParamMultiI dir) <$> fixedAccuracy g
          multiAccPlots = uncurry (plotParamMultiAcc dir) <$> fixedImplementation g


generatePlots :: String -> IO ()
generatePlots outputPath = do
    tests <- readFromCSV outputPath
    --let getPlots :: (Tests Double -> [IO ()]) = fixAll $ \t -> ifOptElse undefined (multi $ ifOpt $ plotAccuracy outputPath t)
    --sequence_ (getPlots tests)
    mapM_ (uncurry (generateTestPlots outputPath)) $ unG tests
    


--creates new folder for output. the folder's name is of format yyyy-mm-dd or, it
--that folder already exists, yyyy-mm-dd(n), where n is the lowest number possible
findOutputPath :: String -> IO String
findOutputPath sourcePath = do
    (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
    let showWith n i = let s = show i in replicate (max 0 $ n - length s) '0' ++ s
    let dir = sourcePath ++ concat (intersperse "-" [showWith 4 year, showWith 2 month, showWith 2 day])
    let suffixes = "" : ["(" ++ show i ++ ")" | i :: Integer <- [1..]]
    let catchAEE a b = a `catch` \e -> if isAlreadyExistsError e then b else throwIO e
    foldr (\p rec -> (createDirectory p >> return (p ++ "/")) `catchAEE` rec) undefined $ (dir ++) <$> suffixes

main :: IO ()
main = do
    outputPath <- findOutputPath "bench/benchmarks/"
    hPutStrLn stderr $ "\nSaving benchmarks to " ++ jsonPath outputPath ++ "\n"
    
    let conf = myConfig outputPath
    result <- try (defaultMainWith conf benchmarks)

    case result of
        Left UserInterrupt -> do
            hPutStrLn stderr "\nUser interrupted benchmarks, compiling plots..."
            generatePlots outputPath
        Left e  -> throwIO e
        Right _ -> do
            hPutStrLn stderr "\nBenchmarks finished, compiling plots..."
            generatePlots outputPath