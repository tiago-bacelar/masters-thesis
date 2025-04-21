module Solver where

import Data.List
import Instances
import Deriv
import Lang
import GHC.TypeNats


--solves for X in the equation X'(t) = F(X(t))
--                             Derivs of F at X0  X(0)     X(t)
solve :: (CompReal r, Powers r) => (Deriv [r]) -> [r] -> r -> [r]
solve f x0 = transpose . map (flip listLimit ???) . transpose . taylor (map . (*)) x
    where x = DCons x0 s DNil   --X'(t) = S(t)
          s = mchain ? ? x       --S(t)  = F(X(t))