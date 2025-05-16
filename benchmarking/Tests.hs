module Tests where

import Data.Ratio

import CompReal

--the leibniz series for pi converges slowly, making it useful for benchmarking
piLeibniz :: (CompReal r) => r
piLeibniz = 4 * seriesSum [(-1) ^ k % (2 * k + 1) | k <- [0..]] (\i -> 2 ^ (max (i-1) 0) - 1)

--this serves as a test for the CompReal instance
--if any of its methods are poorly implemented, 'correctionPi piLeibniz' may return false
correctionPi :: (CompReal r) => r -> [Bool]
correctionPi r = map (containsPi . bound r) [0..50]
    where containsPi (l,u) = fromRational l <= (pi :: Double) && (pi :: Double) <= fromRational u


correctionRational :: (CompReal r) => r -> Rational -> [Bool]
correctionRational r q = map (contains . bound r) [0..]
    where contains (l,u) = l <= q && q <= u