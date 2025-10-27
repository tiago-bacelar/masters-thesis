module Utils where

import GHC.Num
import Data.Bits
import Data.Ratio
import Debug.Trace

traceX :: (Show a) => a -> a
traceX x = trace (show x) x


pow2 :: Int -> Integer
pow2 = shiftL 1

lg2 :: Integer -> Int
lg2 = fromIntegral . integerLogBase 2

logFloor :: Rational -> Int
logFloor r | r < a_r = a - 1
           | otherwise = a
    where a = lg2 (numerator r) - lg2 (denominator r)
          a_r | a < 0 = 1 % pow2 (-a)
              | otherwise = pow2 a % 1


(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(><) f g (x, y) = (f x, g y)

fstOf4 :: (a, b, c, d) -> a
fstOf4 (x,_,_,_) = x


count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

uninterleave :: [a] -> ([a], [a])
uninterleave (x:y:t) = let (xs,ys) = uninterleave t in (x:xs,y:ys)
uninterleave xs = (xs,[])

spanList :: ([a] -> Bool) -> [a] -> ([a], [a]) 
spanList _ [] = ([],[])
spanList f list@(x:xs) | f list    = (x:ys, zs)
                       | otherwise = ([], list)
    where (ys,zs) = spanList f xs

--lists must be ordered. f is applied to the same values multiple times (could be optimized)
mergeOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn f xs [] = xs
mergeOn f [] ys = ys
mergeOn f (x:xs) (y:ys) | f x <= f y = x : mergeOn f xs (y:ys)
                        | otherwise  = y : mergeOn f (x:xs) ys

--inserts an element into an ordered list. if the element already exists in the list, nothing changes
setInsert :: Ord a => a -> [a] -> [a]
setInsert x [] = [x]
setInsert x (h : t) | x < h = x : h : t
                    | x > h = h : setInsert x t
                    | otherwise = h : t

--TODO: make it strict?
replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex i x xs = take i xs ++ x : drop (i+1) xs

--list must be ordered by index and mustn't contain repeated indexes
maybeIndexes :: [(Int,a)] -> [Maybe a]
maybeIndexes xs = rec 0 xs
    where rec n [] = repeat Nothing
          rec n ((i, x) : t) | n == i    = Just x  : rec (n + 1) t
                             | otherwise = Nothing : rec (n + 1) ((i, x) : t)

replaceLast :: (a -> a) -> [a] -> [a]
replaceLast f [x]    = [f x]
replaceLast f (x:xs) = x : replaceLast f xs

findOrLast :: (a -> Bool) -> [a] -> a
findOrLast p [x] = x
findOrLast p (x:xs) | p x = x
                    | otherwise = findOrLast p xs

indexOrLast :: [a] -> Int -> a
indexOrLast xs p = last $ take (p+1) xs