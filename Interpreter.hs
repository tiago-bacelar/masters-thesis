module Interpreter where

import Prelude hiding (Ordering(..))
import Data.Ratio
import Data.Map (Map, (!), empty, insert)

import Hybrid
import Lang

--TODO: errors (operation limit exceeded for zeno points, and precision limit exceeded for comparisons)
--the comparisons problem should be studied closer later on (probably need two modes: strict comparison, which
--guarantees correctness but throws PLE, and lax mode that assumes equality after precision is exhausted)

type State r = Map Identifier r
type RunnableProgram r = HProgram r (State r) --State r -> Hybrid r (State r)


evalL :: (Num r) => LExp r -> State r -> r
evalL e s = sum $ map evalT e
    where evalT (Scalar x) = x
          evalT (Prod x v) = x * (s ! v)

evalB :: (Num r, Ord r) => BTerm r -> State r -> Bool
evalB T _ = True
evalB F _ = False
evalB (LT  x y) s = evalL x s <  evalL y s
evalB (GT  x y) s = evalL x s >  evalL y s
evalB (LEQ x y) s = evalL x s <= evalL y s
evalB (GEQ x y) s = evalL x s >= evalL y s
evalB (EQ  x y) s = evalL x s == evalL y s
evalB (NEQ x y) s = evalL x s /= evalL y s

step :: (Num r, Ord r) => Statement r -> RunnableProgram r
step (Assign v e) s = return $ insert v (evalL e s) s
step (For rs t) s = undefined --TODO
step (IfThenElse c p q) s = if evalB c s then interpret p s else interpret q s
step (WhileDo c p) s = if evalB c s then interpret p s >>= step (WhileDo c p) else return s

interpret :: (Num r, Ord r) => Program r -> RunnableProgram r
interpret = foldl (\p i -> (>>= step i) . p) return


run :: RunnableProgram r -> r -> State r
run p = eval (p empty)

query :: RunnableProgram r -> r -> Identifier -> r
query p = (!) . run p



playI :: IO (Double -> State Double)
playI = do
       input <- readFile "input.txt"
       case parseJaguar input of
              Left err -> error (show err)
              Right ans -> return $ eval (interpret ans empty)