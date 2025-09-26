{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Utils
import CompOrd
import Lang.Parser
import Lang.Interpreter
import Lang.Hybrid
import Solver.Powers
import Plot

import qualified ERA.CReal as ERA --available in stdlib as Data.Numbers.CReal, but that version doesn't export CR, making it kinda useless
import qualified Data.CDAR as CDAR
--import qualified AERN2.Real as AERN2
import qualified Data.Number.IReal as IReal

import Prelude hiding (lookup)
import Data.Char (toLower)
import Data.Map (Map, lookup, assocs)
import Data.Proxy
import System.IO (getContents, hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import System.FilePath.Posix (takeBaseName)
import System.Exit (exitWith, ExitCode(..))

printResult :: (Show a, Show b) => [String] -> a -> RunResult [b] -> IO ()
printResult vars t (Val x) = do
    putStrLn $ "System terminated at t=" ++ show t ++ " with following state:"
    sequence_ [putStrLn (var ++ ": " ++ show val) | (var, val) <- zip vars x]
printResult vars t (Err e) = putStrLn $ "System terminated at t=" ++ show t ++ " with error: " ++ show e


toRunnable :: (Floating r, Powers r, CompOrd r) => [String] -> Program r -> RunnableProgram r
toRunnable vars prog = fmap (fmap takeVars) (interpret prog)
    where takeVars ps = ps {variables = take n (variables ps)}
          n = length vars

--for quick testing with ghci
test :: (Floating r, Powers r, CompOrd r) => IO ([String], RunnableProgram r)
test = do
    input <- readFile "input.txt"
    case parseJaguar input of
            Failed err -> error ("Parse error: " ++ show err) --parse error
            Ok (vars, code) -> return (vars, toRunnable vars code)

play :: IO (IReal.IReal -> RunResult [IReal.IReal])
play = fmap (\(_,prog) -> query prog 20 300) test


data Mode = Plot | Query | InteractivePlot deriving (Show, Eq)
data SomeProxy where SomeProxy :: forall r. (Plottable r r, Floating r, Powers r, CompOrd r) => Proxy r -> SomeProxy
data Options = Options  { optFile       :: Maybe String
                        , optNumType    :: SomeProxy
                        , optCompPrec   :: Int
                        , optIterations :: Integer
                        , optPlotConfig :: PlotConfig
                        , optMode       :: Mode
                        }

startOptions :: Maybe String -> Options
startOptions f = Options    { optFile       = f
                            , optNumType    = SomeProxy (Proxy :: Proxy IReal.IReal)
                            , optCompPrec   = 32
                            , optIterations = 200
                            , optPlotConfig = defPlotConfig
                            , optMode       = Plot
                            }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "t" ["type"]
        (ReqArg
            (\arg opt -> case map toLower arg of
                            "era"    -> return opt { optNumType = SomeProxy (Proxy :: Proxy ERA.CReal)      }
                            "cdar"   -> return opt { optNumType = SomeProxy (Proxy :: Proxy CDAR.CR)        }
                        --  "aern"   -> return opt { optNumType = SomeProxy (Proxy :: Proxy AERN.RealNumber)}
                            "ireal"  -> return opt { optNumType = SomeProxy (Proxy :: Proxy IReal.IReal)    }
                            "double" -> return opt { optNumType = SomeProxy (Proxy :: Proxy Double)         }
                            _        -> error "TODO")
            "TYPE")
        "Number type"
    , Option "c" ["comparison-precision"]
        (ReqArg
            (\arg opt -> return opt { optCompPrec = read arg })
            "INT")
        "Comparison precision"
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

    --TODO: plot config options (incompatible with modes other than Plot)

    , Option "q" ["query"] --TODO: must have an input file
        (NoArg
            (\opt -> return opt { optMode = Query }))
        "Query mode"
    , Option "i" ["interactive"] --TODO: must have an input file???? (depends on how i process the input)
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
                        Ok (vars, code :: Program r) -> return (vars, toRunnable vars code)

    let (system, discs) = run prog compPrec iterations

    case mode of
        Plot -> do
                    --TODO: printResult vars tf $ fmap snd $ endpoint system
                    plotHybrid plotConfig vars system discs
        Query -> undefined
        InteractivePlot -> undefined


plot :: IO ()
plot = mainWith Options { optFile       = Just "input.txt"
                        , optNumType    = SomeProxy (Proxy :: Proxy IReal.IReal)
                        , optCompPrec   = 10
                        , optIterations = 30
                        , optPlotConfig = defPlotConfig { rangeT = Just (0,1), rangeX = Just (0,1), precision = 6 }
                        , optMode       = Plot
                        }


--TODO: query mode (query values, set precision, etc)
--TODO: interactive plot (janela a parte que da para fazer zoom, mover e tal)

--plot config: { vars: [String], minT: Maybe a, maxT: Maybe a, minX: Maybe a, maxX: Maybe a } --right and left axis??