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
play = fmap (\p -> query p 20 300) test

plot :: IO ()
plot = do
    let vars = ["a", "b", "cols"]
    let lookupVars st = map (`lookup` st) vars
    --p <- test :: IO (RunnableProgram Double)
    p <- test :: IO (RunnableProgram IReal)
    let (h, ds) = run p 20 300
    let system = fmap (fmap lookupVars) h
    let discs = map (\(t,s,s') -> (t, lookupVars s, lookupVars s')) ds
    --plotHybrid vars h discs
    plotHybridCR vars system discs 4



main :: IO () --TODO: cmd line args, etc etc
main = do
    f <- play
    plot
    mapM_ (\(t, x) -> putStrLn $ show t ++ ": " ++ show x) $ map (\t -> (t, f t)) [0..]