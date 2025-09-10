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

printResult :: (Show a, Show b) => [String] -> a -> RunResult [b] -> IO ()
printResult vars t (Val x) = do
    putStrLn $ "System terminated at t=" ++ show t ++ " with following state:"
    sequence_ [putStrLn (var ++ ": " ++ show val) | (var, val) <- zip vars x]
printResult vars t (Err e) = putStrLn $ "System terminated at t=" ++ show t ++ " with error: " ++ show e


--for quick testing with ghci
test :: (Floating a, Powers a, CompOrd a, Show a) => IO ([String], RunnableProgram a)
test = do
    input <- readFile "input.txt"
    case parseJaguar input of
            Failed err -> error ("Parse error: " ++ show err) --parse error
            Ok (vars, prog) -> return (vars, interpret prog)

play :: IO (IReal -> RunResult [IReal])
play = fmap (\(_,prog) -> query prog 20 300) test

plot :: IO ()
plot = do
    -- (vars, prog) <- test :: IO ([String], RunnableProgram Double)
    (vars, prog) <- test :: IO ([String], RunnableProgram IReal)

    let (system, discs) = run prog 20 300

    case duration system of
        Just tf -> do
                    printResult vars tf $ fmap snd $ endpoint system
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