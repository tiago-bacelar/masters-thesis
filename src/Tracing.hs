{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
Tracing tools for debugging
-}

module Tracing (module Tracing, trace) where

import CompOrd

import Debug.Trace (trace)


doTracing :: Bool
doTracing = True

myTrace :: String -> a -> a
myTrace s x = if doTracing then trace s x else x



traceWith :: String -> (a -> String) -> a -> a
traceWith s f x = myTrace (s ++ ": " ++ f x) x

traceX :: (Show a) => String -> a -> a
traceX s = traceWith s show

traceListWith :: String -> (a -> String) -> [a] -> [a]
traceListWith s f xs = myTrace (s ++ ":") $ map (\(i,x) -> myTrace ("[" ++ show i ++ "]" ++ f x) x) $ zip ([0..] :: [Integer]) xs

traceListX :: (Show a) => String -> [a] -> [a]
traceListX s = traceListWith s show


bsOrd :: (Fractional r, CompOrd r) => Rational -> Rational -> r -> Double
bsOrd l r x | r - l < 0.000001 = fromRational m
            | (x <! fromRational m) 20 = bsOrd l m x
            | otherwise = bsOrd m r x
            where m = (l + r) / 2

showCR :: (Fractional r, CompOrd r) => r -> String
showCR x = show (bsOrd (-1000) 1000 x)

traceCR :: (Fractional r, CompOrd r) => String -> r -> r
traceCR s = traceWith s showCR

traceListCR :: (Fractional r, CompOrd r) => String -> [r] -> [r]
traceListCR s = traceListWith s showCR


--To facilitate tracing structures containing functions
instance Show (a -> b) where
    show _ = "<FUNC>"