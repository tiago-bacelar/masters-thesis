module Tests where

import Data.Ratio
import Data.Map((!))

import CompOrd
import CompReal
import Solver.Powers
import Solver.Interval
import Lang.Parser
import Lang.Interpreter

--the leibniz series for pi converges slowly, making it useful for benchmarking
piLeibniz :: (CompReal r) => r
piLeibniz = 4 * seriesSumRatio [(-1) ^ k % (2 * k + 1) | k <- [0..]] (\i -> 2 ^ (max (i-1) 0) - 1)

--this serves as a test for the CompReal instance
--if any of its methods are poorly implemented, 'correctionPi piLeibniz' may return false
correctionPi :: (CompReal r) => r -> [Bool]
correctionPi r = map (containsPi . bound r) [0..50]
    where containsPi (l,u) = fromRational l <= (pi :: Double) && (pi :: Double) <= fromRational u


correctionRational :: (CompReal r) => r -> Rational -> [Bool]
correctionRational r q = map (contains . bound r) [0..]
    where contains (l,u) = l <= q && q <= u


ballBounce :: (CompOrd r, CompReal r, Powers r, Show r) => Rational -> r
ballBounce t = fromVal (query (interpret prog) (16, 100) (fromRational t)) ! "y"
    where prog = Seq (Assign "y" (Num $ AnyFloat 0)) $ Seq (Assign "v" (Num $ AnyFloat 1)) $ loop
          loop = WhileDo (Term $ BConst True) (Seq arc bounce)
          arc = For [("y", Var $ V "v"), ("v", Num $ AnyFloat $ -1)] (Just $ Op Mult (Num $ AnyFloat 2) (Var $ V "v"))
          bounce = Assign "v" (Op Mult (Num $ AnyFloat $ -0.8) (Var $ V "v"))