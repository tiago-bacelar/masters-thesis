
import Lang.Parser
import Lang.Interpreter

--for quick testing with ghci
play :: IO (Double -> State Double)
play = do
       input <- readFile "input.txt"
       case fmap (run . interpret) (parseJaguar input) of
              Failed err -> error (show err)
              Ok ans -> return ans


main :: IO ()
main = return () --TODO