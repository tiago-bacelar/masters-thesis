module Tracing (module Tracing, trace) where

import CompOrd

import Debug.Trace (trace)

traceWith :: (a -> String) -> a -> a
traceWith f x = trace (f x) x

traceX :: (Show a) => String -> a -> a
traceX s x = trace (s ++ ": " ++ show x) x

traceList :: String -> (a -> String) -> [a] -> [a]
traceList s f xs = trace (s ++ ":") $ foldr trace xs $ map (\(i,x) -> "["++show i++"]"++f x) $ zip ([0..] :: [Integer]) xs



bsOrd :: (Fractional r, CompOrd r) => Rational -> Rational -> r -> Double
bsOrd l r x | r - l < 0.000001 = fromRational m
            | (x <! fromRational m) 20 = bsOrd l m x
            | otherwise = bsOrd m r x
            where m = (l + r) / 2

traceCR :: (Fractional r, CompOrd r) => String -> r -> r
traceCR s x = trace (s ++ ": " ++ show (bsOrd (-1000) 1000 x)) x