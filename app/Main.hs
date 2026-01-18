{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Utils
import Lang.Parser
import Lang.Hybrid hiding (Query)
import Lang.Interpreter
import Plot

import qualified CompReal.Instances.CDAR as CDAR
import qualified CompReal.Instances.AERN2 as AERN2
import qualified CompReal.Instances.ERA as ERA
import qualified CompReal.Instances.ExactReal as ExactReal
import qualified CompReal.Instances.IReal as IReal

import Prelude hiding (lookup)
import Data.Char (toLower)
import Data.Proxy
import GHC.Data.Maybe (fromJust, rightToMaybe)
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))

expectPositive :: (Num a, Ord a, Show a) => a -> IO ()
expectPositive x | x > 0     = return ()
                 | otherwise = do
                    hPutStrLn stderr $ "Expected positive value but got " ++ show x
                    exitWith $ ExitFailure 1

expectNonNegative :: (Num a, Ord a, Show a) => a -> IO ()
expectNonNegative x | x >= 0    = return ()
                    | otherwise = do
                        hPutStrLn stderr $ "Expected non-negative value but got " ++ show x
                        exitWith $ ExitFailure 1

expectAtMost :: Int -> String -> IO [String]
expectAtMost n s | length l <= n = return l
                 | otherwise     = do
                    hPutStrLn stderr $ "Expected at most " ++ show n ++ "elements but got '" ++ s ++ "'"
                    exitWith $ ExitFailure 1
    where l = strSplit ',' s

expectExactly :: Int -> String -> IO [String]
expectExactly n s | length l == n = return l
                  | otherwise     = do
                        hPutStrLn stderr $ "Expected exactly " ++ show n ++ "elements but got '" ++ s ++ "'"
                        exitWith $ ExitFailure 1
    where l = strSplit ',' s


--TODO: replace Show restriction with something else? (to have consistent formats between implementations)
printState :: (Show r) => [String] -> [r] -> IO ()
printState vars vals = sequence_ [putStrLn (var ++ ": " ++ show val) | (var, val) <- zip vars vals]

printUnroll :: (Show t, Show r) => [String] -> t -> Either (Error [r]) [r] -> IO ()
printUnroll vars t (Left (err, s)) = do
    putStrLn $ "System terminated at t=" ++ show t ++ " with error: " ++ show err
    putStrLn "At time of error, the system had the following state:"
    printState vars s
printUnroll vars t (Right s) = do
    putStrLn $ "System terminated at t=" ++ show t ++ " with following state:"
    printState vars s



type AppNum r = (SimNum r, Plottable r r, Show r)
data Mode = Plot | Query deriving (Show, Eq) --TODO: InteractivePlot
data SomeProxy where SomeProxy :: forall r. AppNum r => Proxy r -> SomeProxy
data Options = Options  { optFile       :: Maybe String
                        , optNumType    :: SomeProxy        -- -n
                        , optCompAcc    :: Maybe Int        -- -c
                        , optIterations :: Maybe Integer    -- -l
                        , optPlotVars   :: S.Set String     -- -v (comma separated)
                        , optPlotConfig :: PlotConfig
                        , optMode       :: Mode             -- -q for query mode (default is plot)
                        }

startOptions :: Maybe String -> Options
startOptions f = Options    { optFile       = f
                            , optNumType    = SomeProxy (Proxy :: Proxy CDAR.CR) --TODO: change default?
                            , optCompAcc    = Just 32
                            , optIterations = Nothing
                            , optPlotVars   = S.empty
                            , optPlotConfig = defPlotConfig
                            , optMode       = Plot
                            }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "n" ["numeric-type"]
        (ReqArg
            (\arg opt -> case map toLower arg of
                "cdar"      -> return opt { optNumType = SomeProxy (Proxy :: Proxy CDAR.CR)             }
                "exact-real"-> return opt { optNumType = SomeProxy (Proxy :: Proxy ExactReal.AnyCReal)  }
                "era"       -> return opt { optNumType = SomeProxy (Proxy :: Proxy ERA.CReal)           }
                "aern2"     -> return opt { optNumType = SomeProxy (Proxy :: Proxy AERN2.CReal)         }
                "ireal"     -> return opt { optNumType = SomeProxy (Proxy :: Proxy IReal.IReal)         }
                "double"    -> return opt { optNumType = SomeProxy (Proxy :: Proxy Double)              }
                _           -> do
                    hPutStrLn stderr $ "Numeric type not recognized: " ++ arg
                    hPutStrLn stderr $ "Valid types are (case insensitive): cdar, exact-real, era, aern2, ireal, double"
                    exitWith $ ExitFailure 1
            )
            "TYPE")
        "Number type. Default is CDAR"
    , Option "c" ["comparison-accuracy"]
        (ReqArg
            (\arg opt -> let c = read arg in expectNonNegative c >> return opt { optCompAcc = Just c })
            "INT")
        "Comparison accuracy. Default is 32"
    , Option "l" ["loop-iterations"]
        (ReqArg
            (\arg opt -> let l = read arg in expectNonNegative l >> return opt { optIterations = Just l })
            "INT")
        "Max iterations. Default is infinite"
    , Option "v" ["variables"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> return opt { optPlotVars = S.fromList $ strSplit ',' arg })
            "VAR1,VAR2...")
        "Variables to plot. Default is all"
    , Option "o" ["output"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> return opt { optPlotConfig = (optPlotConfig opt) { outputPath = arg }})
            "FILE")
        "Output path"
    , Option "s" ["samples"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> let s = read arg in expectPositive s >> return opt { optPlotConfig = (optPlotConfig opt) { sampleNo = s }})
            "INT")
        "Number of samples to plot. Default is 500"
    , Option "a" ["accuracy"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> let a = read arg in expectNonNegative a >> return opt { optPlotConfig = (optPlotConfig opt) { queryAccuracy = Just a, realAccuracy = a }})
            "INT")
        "Accuracy of each sample to plot. Default is 32"
    , Option "t" ["time-range"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> do
                ts <- map readDecimal <$> expectAtMost 2 arg
                mapM_ expectNonNegative ts
                let range = if length ts == 1 then (0, ts !! 0) else (ts !! 0, ts !! 1)
                return opt { optPlotConfig = (optPlotConfig opt) { rangeT = Just range }})
            "DECIMAL | DECIMAL,DECMIAL")
        "Range of times to plot. Default is (0,duration)"
    , Option "x" ["value-range"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> do
                [xl, xu] <- map readDecimal <$> expectExactly 2 arg
                return opt { optPlotConfig = (optPlotConfig opt) { rangeX = Just (xl, xu) }})
            "Range of values to plot")
        "DECIMAL,DECMIAL"
    , Option "q" ["query"]
        (NoArg
            (\opt -> return opt { optMode = Query }))
        "Query mode"
    {-
    , Option "i" ["interactive"] --input file? (could maybe read from stdin depending on how input is processed)
        (NoArg
            (\opt -> return opt { optMode = InteractivePlot }))
        "Interactive plot"
    , Option "" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
    -}
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, _) = getOpt RequireOrder options args
    file <- case nonOptions of
                []  -> return Nothing
                [f] -> return (Just f)
                _   -> do
                        hPutStrLn stderr "Only one input file supported"
                        exitWith $ ExitFailure 1

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return $ startOptions file) actions
    mainWith opts


mainWith :: Options -> IO ()
mainWith (Options   { optFile       = file
                    , optNumType    = numType
                    , optCompAcc    = compAcc
                    , optIterations = iterations
                    , optPlotVars   = plotVars
                    , optPlotConfig = plotConfig
                    , optMode       = mode }) = case numType of
  SomeProxy (_ :: Proxy r) -> do
    input <- maybe getContents readFile file

    (vars, prog) <- case parseJaguar input of
                        Failed err -> error ("Parse error: " ++ show err)
                        Ok (vars, code :: Program r) -> return (vars, interpret code)

    let (system, discs) = run prog (length vars) compAcc iterations

    case mode of
      Plot -> do
        let varIndexes = [i | (i,v) <- zip [0..] vars, S.member v plotVars]
        let filterIndexes :: [a] -> [a]
            filterIndexes = if S.null plotVars then id else (getIndexes varIndexes)

        let filteredVars = filterIndexes vars
        let filteredSystem = smapCH (id >< filterIndexes) $ fmap (fmap (id >< filterIndexes) . rightToMaybe) system
        let filteredDiscs = map (\(w,x,y,z) -> (w,x,y,filterIndexes z)) . discs

        plotHybrid plotConfig filteredVars filteredSystem filteredDiscs
        case rangeT plotConfig of
            Just _  -> return ()
            Nothing -> uncurry (printUnroll vars) $ fromJust $ unrollCH $ fmap (fmap snd -|- snd) system
      Query -> undefined --TODO





--for quick testing with ghci
test :: (SimNum r) => IO ([String], RunnableProgram r)
test = do
    input <- readFile "input.txt"
    case parseJaguar input of
            Failed err -> error ("Parse error: " ++ show err)
            Ok (vars, code) -> return (vars, interpret code)

type TestType = CDAR.CR

play :: IO (TestType -> Int -> [[TestType]])
play = fmap (\(vars,prog) -> NE.toList .-. runQueryJust . query prog (length vars) (Just 20) Nothing) test

playDiscs :: IO [(TestType, Step, Step, [Maybe (TestType, TestType)])]
playDiscs = fmap (\(vars,prog) -> snd (run prog (length vars) (Just 20) Nothing) Nothing) test

plot :: IO ()
plot = mainWith Options { optFile       = Just "input.txt"
                        , optNumType    = SomeProxy (Proxy :: Proxy TestType)
                        , optCompAcc    = Just 20
                        , optIterations = Just 300
                        , optPlotVars   = S.fromList []
                        , optPlotConfig = defPlotConfig { realAccuracy = 10 }
                        , optMode       = Plot
                        }


--TODO: query mode (perform queries, set accuracy, etc (plot with current settings???))
--TODO: interactive plot (janela a parte que da para fazer zoom, mover e tal)