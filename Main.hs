import CompOrd
import Lang.Parser
import Lang.Interpreter
import Lang.Hybrid
import Solver.Powers
import Plot

import Prelude hiding (lookup)
import Data.Map (Map, lookup)
import Data.Number.IReal (IReal)

--for quick testing with ghci
test :: (Floating a, Powers a, CompOrd a, Show a) => IO (RunnableProgram a)
test = do
    input <- readFile "input.txt"
    case parseJaguar input of
            Failed err -> error ("Parse error: " ++ show err) --parse error
            Ok ans -> return (interpret ans)

play :: IO (IReal -> RunResult (Map Ident IReal))
play = fmap (`query` (10, 100)) test

plot :: IO ()
plot = do
    let vars = ["y", "v", "a"]
    --p <- test :: IO (RunnableProgram Double)
    p <- test :: IO (RunnableProgram IReal)
    let h = fmap (fmap (\st -> map (`lookup` st) vars)) $ run p (10, 100)
    --plotHybrid vars h
    plotHybridCR vars h 4



main :: IO () --TODO: cmd line args, etc etc
main = do
    f <- play
    plot
    mapM_ (\(t, x) -> putStrLn $ show t ++ ": " ++ show x) $ map (\t -> (t, f t)) [0..]