{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
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
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))

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

--for quick testing with ghci
test :: (SimNum r) => IO ([String], RunnableProgram r)
test = do
    input <- readFile "input.txt"
    case parseJaguar input of
            Failed err -> error ("Parse error: " ++ show err) --parse error
            Ok (vars, code) -> return (vars, interpret code)

play :: IO (TestType -> [TestType])
play = fmap (\(vars,prog) -> head . (`runQuery` 20) . query prog (length vars) (Just 20) (Just 300)) test


data Mode = Plot | Query | InteractivePlot deriving (Show, Eq)
data SomeProxy where SomeProxy :: forall r. (Plottable r r, SimNum r, Show r) => Proxy r -> SomeProxy
data Options = Options  { optFile       :: Maybe String
                        , optNumType    :: SomeProxy
                        , optCompPrec   :: Maybe Int
                        , optIterations :: Maybe Integer
                        , optPlotConfig :: PlotConfig
                        , optMode       :: Mode
                        }

startOptions :: Maybe String -> Options
startOptions f = Options    { optFile       = f
                            , optNumType    = SomeProxy (Proxy :: Proxy IReal.IReal)
                            , optCompPrec   = Just 32
                            , optIterations = Just 200
                            , optPlotConfig = defPlotConfig
                            , optMode       = Plot
                            }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "t" ["type"]
        (ReqArg
            (\arg opt -> case map toLower arg of
                            "cdar"      -> return opt { optNumType = SomeProxy (Proxy :: Proxy CDAR.CR)             }
                            "exact-real"-> return opt { optNumType = SomeProxy (Proxy :: Proxy ExactReal.AnyCReal)  }
                            "era"       -> return opt { optNumType = SomeProxy (Proxy :: Proxy ERA.CReal)           }
                            "aern"      -> return opt { optNumType = SomeProxy (Proxy :: Proxy AERN2.CReal)         }
                            "ireal"     -> return opt { optNumType = SomeProxy (Proxy :: Proxy IReal.IReal)         }
                            "double"    -> return opt { optNumType = SomeProxy (Proxy :: Proxy Double)              }
                            _           -> error "TODO: error msg")
            "TYPE")
        "Number type"
    , Option "c" ["comparison-accuracy"]
        (ReqArg
            (\arg opt -> return opt { optCompPrec = read arg })
            "INT")
        "Comparison accuracy"
    , Option "n" ["iterations"]
        (ReqArg
            (\arg opt -> return opt { optIterations = read arg })
            "INT")
        "Max iterations"
    , Option "o" ["output"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> return opt { optPlotConfig = (optPlotConfig opt) { outputPath = arg }})
            "FILE")
        "Output path"
    , Option "o" ["output"] --TODO: incompatible with modes other than Plot
        (ReqArg
            (\arg opt -> return opt { optPlotConfig = (optPlotConfig opt) { outputPath = arg }})
            "FILE")
        "Output path"

    --TODO: plot config options (incompatible with modes other than Plot (and interactive plot??))

    , Option "q" ["query"] --TODO: must have an input file
        (NoArg
            (\opt -> return opt { optMode = Query }))
        "Query mode"
    , Option "i" ["interactive"] --TODO: must have an input file???? (could maybe read from stdin depending on how i process the input)
        (NoArg
            (\opt -> return opt { optMode = InteractivePlot }))
        "Interactive plot"
    {-
    , Option "V" ["version"]
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
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    file <- case nonOptions of
                []  -> return Nothing
                [f] -> return (Just f)
                _   -> error "TODO"

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return $ startOptions file) actions
    mainWith opts


mainWith :: Options -> IO ()
mainWith (Options   { optFile       = file
                    , optNumType    = numType
                    , optCompPrec   = compPrec
                    , optIterations = iterations
                    , optPlotConfig = plotConfig
                    , optMode       = mode }) = case numType of
  SomeProxy (_ :: Proxy r) -> do
    input <- maybe getContents readFile file

    (vars, prog) <- case parseJaguar input of
                        Failed err -> error ("Parse error: " ++ show err)
                        Ok (vars, code :: Program r) -> return (vars, interpret code)

    let (system, discs) = run prog (length vars) compPrec iterations

    case mode of
        Plot -> do
                    plotHybrid plotConfig vars (fmap rightToMaybe system) discs
                    case rangeT plotConfig of
                        Just _  -> return ()
                        Nothing -> uncurry (printUnroll vars) $ fromJust $ unrollCH $ fmap (fmap snd -|- snd) system
        Query -> undefined
        InteractivePlot -> undefined


type TestType = CDAR.CR
plot :: IO ()
plot = mainWith Options { optFile       = Just "input.txt"
                        , optNumType    = SomeProxy (Proxy :: Proxy TestType)
                        , optCompPrec   = Just 10
                        , optIterations = Just 30
                        , optPlotConfig = defPlotConfig { accuracy = 6 }
                        , optMode       = Plot
                        }


--TODO: query mode (perform queries, set accuracy, etc)
--TODO: interactive plot (janela a parte que da para fazer zoom, mover e tal)