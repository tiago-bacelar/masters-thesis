import Utils
import CompOrd
import Lang.Parser
import Lang.Interpreter
import Lang.Hybrid
import Solver.Powers
import Plot

import Prelude hiding (lookup)
import Data.Map (Map, lookup, assocs)
import Data.Number.IReal (IReal)

printResult :: (Show a, Show b) => a -> RunResult (Map Ident b) -> IO ()
printResult t (Val x) = do
    putStrLn $ "System terminated at t=" ++ show t ++ " with following state:"
    sequence_ [putStrLn (var ++ ": " ++ show val) | (var, val) <- assocs x]
printResult t (Err e) = putStrLn $ "System terminated at t=" ++ show t ++ " with error: " ++ show e


--for quick testing with ghci
test :: (Floating a, Powers a, CompOrd a, Show a) => IO (RunnableProgram a)
test = do
    input <- readFile "input.txt"
    case parseJaguar input of
            Failed err -> error ("Parse error: " ++ show err) --parse error
            Ok ans -> return (interpret ans)

play :: IO (IReal -> RunResult (Map Ident IReal))
play = fmap (\p -> query p 20 300) test

plot :: IO ()
plot = do
    let vars = ["y", "v"]
    let lookupVars st = map (`lookup` st) vars

    --p <- test :: IO (RunnableProgram Double)
    p <- test :: IO (RunnableProgram IReal)
    
    let (h, ds) = run p 20 300
    let system = fmap (fmap (id >< lookupVars)) h
    let discs = map (\(t,(i,s),(j,s')) -> (t, (i, lookupVars s), (j, lookupVars s'))) ds

    case duration h of
        Just tf -> do
                    printResult tf $ fmap snd $ endpoint h
                    --plotHybrid vars system discs
                    plotHybridCR vars system discs 4
        Nothing -> putStrLn "Only finite systems support plotting"


main :: IO () --TODO: cmd line args, etc etc
main = do
    f <- play
    plot
    mapM_ (\(t, x) -> putStrLn $ show t ++ ": " ++ show x) $ map (\t -> (t, f t)) [0..]

--TODO: query mode (query values, set precision, etc)
--TODO: generate plot
--TODO: interactive plot (janela a parte que da para fazer zoom, mover e tal)