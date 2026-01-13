{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generators (module Generators) where

import Data.List (tails)
import Data.Ratio ((%))
import Data.Functor.Identity (Identity)
import Test.SmallCheck.Series

import Utils
import Powers
import CompReal


--Effectively equivalent to NonNegative, but I wanted my own newtype if I ever needed to change it
newtype Acc = Acc Int deriving (Show, Num, Eq, Ord)
instance (Monad m) => Serial m Acc where
    series = generate $ \d -> map Acc $ take (d+1) [0..]


--This is used to generate Rationals linearly with depth, instead of squared
--A depth of 9 will generate 10 Rats
--Usually, a depth of 9 would generate 10 numerators and 10 denominators, resulting in 100 rationals
newtype Rat = Rat Rational deriving (Show, Num, Eq, Ord)
instance (Monad m) => Serial m Rat where
    --There's probably an idiomatic way of doing this with Series, idk
    series = generate $ \d -> map Rat $ take (d+1) rats
        where rats = concat $ leftDiagonals [[n % d | n <- ints] | d <- [1..]]
              ints = interleave [0,-1..] [1..]

newtype Distinct2 a = Distinct2 (a,a) deriving (Show)
instance (Monad m, Serial Identity a) => Serial m (Distinct2 a) where
    series = generate $ \d -> concat $ map aux $ tails $ listSeries d
        where aux [] = []
              aux (x:xs) = map (Distinct2 . (x,)) xs

--given the limit of a sequence, returns a normalized sequence where term n is an accuracy n approximation
--if the sequence is finite, the limit is appended to the sequence as its last element
normalize :: Rational -> [Rational] -> [Rational]
normalize l = aux (1%2)
    where aux e (x:xs) | l - x <= e = x : aux (e/2) (x:xs)
                       | otherwise  = aux e xs
          aux _ [] = [l]

--the sum 1/2 + 1/4 + 1/8 + .... = 1
geometricSeriesDyad :: [Rational]
geometricSeriesDyad = scanl1 (+) $ iterate (/2) (1%2)

--the sum 1/3 + 1/9 + 1/27 + ... = 1/2
geometricSeries :: [Rational]
geometricSeries = normalize (1%2) $ scanl1 (+) $ iterate (/3) (1%3)

finiteList :: [Rational]
finiteList = [-0.2, 0.045, 1%6, 2%7]


--1
geometricSeriesDyadRat :: (CompReal r) => r
geometricSeriesDyadRat = listCons geometricSeriesDyad

--2
geometricSeriesDyadCR :: (CompReal r) => r
geometricSeriesDyadCR = listLimit $ fromRational . (1+) <$> geometricSeriesDyad

--0.5
geometricSeriesRat :: (CompReal r) => r
geometricSeriesRat = listCons geometricSeries

--1.5
geometricSeriesCR :: (CompReal r) => r
geometricSeriesCR = listLimit $ fromRational . (1+) <$> geometricSeries

--2/7
finiteListRat :: (CompReal r) => r
finiteListRat = listCons finiteList

--9/7
finiteListCR :: (CompReal r) => r
finiteListCR = listLimit $ fromRational . (1+) <$> finiteList


newtype CRS r = CRS r deriving (Show)
instance {-# OVERLAPPABLE #-} (Monad m, CompReal r, Powers r) => Serial m (CRS r) where
    --Can't contain repeated elements to ensure Distinct2 works properly
    series = generate $ \_ -> CRS <$>
        [ 0, 0.25, -0.125, 0.001, -0.2
        , fromRational (1%3), fromRational (3%7), fromRational (873084756%48762367)
        , pi, exp 1, sqrt 2, 2 ** fromRational (1%3), sin 1
        , geometricSeriesDyadRat
        , geometricSeriesDyadCR
        , geometricSeriesRat
        , geometricSeriesCR
        , finiteListRat
        , finiteListCR
        , pow (fromRational (2%3)) 10
        ]