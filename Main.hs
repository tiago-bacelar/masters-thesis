import Lang.Parser
import Lang.Interpreter
import Lang.Hybrid
import Solver.Powers
import Plot

import Data.Map (Map, (!))
import Data.Number.IReal (IReal)

--for quick testing with ghci
test :: (Floating a, Powers a, Ord a) => IO (RunnableProgram a)
test = do
    input <- readFile "input.txt"
    case parseJaguar input of
            Failed err -> error ("Parse error: " ++ show err) --parse error
            Ok ans -> return (interpret ans)

play :: IO (IReal -> Map Ident IReal)
play = fmap run test

plot :: IO ()
plot = do
    p <- test :: IO (RunnableProgram Double)
    let h = fmap snd (p initial)
    let vars = ["y", "v"]
    plotHybrid vars (fmap (\st -> map (st!) vars) h)



main :: IO () --TODO
main = do
    f <- play
    plot
    mapM_ (\(t, x) -> putStrLn $ show t ++ ": " ++ show x) $ map (\t -> (t, f t)) [0..]