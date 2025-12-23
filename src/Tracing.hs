module Tracing (module Tracing, trace) where

import CompOrd

import Debug.Trace (trace)


doTracing :: Bool
doTracing = False

aux :: a -> a -> a
aux x y = if doTracing then y else x



traceWith :: (a -> String) -> a -> a
traceWith f x = aux x $ trace (f x) x

traceX :: (Show a) => String -> a -> a
traceX s x = aux x $ trace (s ++ ": " ++ show x) x

traceList :: String -> (a -> String) -> [a] -> [a]
traceList s f xs = aux xs $ trace (s ++ ":") $ foldr trace xs $ map (\(i,x) -> "["++show i++"]"++f x) $ zip ([0..] :: [Integer]) xs



bsOrd :: (Fractional r, CompOrd r) => Rational -> Rational -> r -> Double
bsOrd l r x | r - l < 0.000001 = fromRational m
            | (x <! fromRational m) 20 = bsOrd l m x
            | otherwise = bsOrd m r x
            where m = (l + r) / 2

showCR :: (Fractional r, CompOrd r) => r -> String
showCR x = show (bsOrd (-1000) 1000 x)

traceCR :: (Fractional r, CompOrd r) => String -> r -> r
traceCR s x = aux x $ trace (s ++ ": " ++ showCR x) x