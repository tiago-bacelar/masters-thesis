module Utils where

import GHC.Num
import Data.Bits
import Data.Ratio
import GHC.Utils.Misc (thdOf3)


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

(.-.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-.) f g x y = f (g x y)
infixr 9 .-.


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

joinWith :: (a -> a -> a) -> [a] -> [a] -> [a]
joinWith f (x:xs) (y:ys) = f x y : joinWith f xs ys
joinWith f xs [] = xs
joinWith f [] ys = ys

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

dropOrLast :: Int -> [a] -> [a]
dropOrLast _ [x] = [x]
dropOrLast 0 xs = xs
dropOrLast p (_:xs) = dropOrLast (p-1) xs

indexOrLast :: [a] -> Int -> a
indexOrLast xs p = last $ take (p+1) xs


--Balanced fold, minimizing depth of call tree. Assumes associative operator.
--This is useful for CompReals because operations usually try to balance errors by
--splitting it equally among the two terms. Therefore, if an operation is applied across
--a list, the first element will take half of the error, the second will take a quarter, etc
--This function ensures the error is distributed equally across all elements of the list
foldTree :: (a -> a -> a) -> a -> [a] -> a
foldTree f u []        = u
foldTree f u (x:xs)    = foldTree f (f u x) (g xs)
  where g (x:y:xs)  = f x y : g xs
        g xs        = xs

-- Balanced fold for associative operator over non-empty list.
foldTree1 :: (a -> a -> a) -> [a] -> a
foldTree1 f (x:xs) = foldTree f x xs

--same as foldTree1, but generates all partial results (from the left)
--evaluating the partial results may require additional applications of f
-- (e.g. scanlTree1 (+) [1,2,3,4] will return [1, 1+2, (1+2)+3, (1+2)+(3+4)]
-- notice how the value (1+2)+3 isn't used in the next term)
scanlTree1 :: (a -> a -> a) -> [a] -> [a]
scanlTree1 f = map (thdOf3 . head) . tail . scanl (aux 0) []
    where aux i [] x = [(x,i,x)]
          aux i ((y,j,acc):ys) x | i < j = (x,i,f x acc) : (y,j,acc) : ys
                                 | otherwise = aux (i+1) ys (f x y)