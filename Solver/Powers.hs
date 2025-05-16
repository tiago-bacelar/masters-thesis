module Solver.Powers where

--This class was taken and adapted from https://github.com/sydow/ireal/blob/master/Data/Number/IReal/Powers.hs

-- | Common functions collected to allow for instances which
-- handle dependency problems for intervals, and for automatic
-- differentiation.
class Num a => Powers a where
   -- squaring function; in @sq x@, there is only one occurrence of @x@ (as opposed to @x * x@)
   sq :: a -> a
   -- power function; @pow n x@ computes @x^n@, but can be implemented for 'IReal' with correct
   -- treatment of dependency.
   pow :: a -> Int -> a
   -- list of powers; a more efficient alternative to @pow@ when all powers of @x@ are used
   powers :: a -> [a]
   sq = flip pow 2
   pow x n = x ^ n
   powers x = iterate (*x) 1

instance Powers Double
instance Powers Integer