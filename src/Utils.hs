module Utils (module Utils) where

import GHC.Num
import Data.Bits
import Data.List
import Data.Ratio
import qualified Data.IntMap as IM
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
infix 5 ><

(-|-) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
(-|-) f g (Left x) = Left $ f x
(-|-) f g (Right y) = Right $ g y
infix 4 -|-

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
joinWith _ xs [] = xs
joinWith _ [] ys = ys

split :: (Eq a) => a -> [a] -> [[a]]
split x [] = []
split x (y:ys) | x == y    = [] : split x ys
               | otherwise = appendHead y $ split x ys
    where appendHead y [] = [[y]]
          appendHead y (ys:yss) = (y : ys) : yss

--lists must be ordered. f is applied to the same values multiple times (could be optimized)
mergeOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ xs [] = xs
mergeOn _ [] ys = ys
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

--indexes must be increasing
getIndexes :: [Int] -> [a] -> [a]
getIndexes []       = const []
getIndexes (i:is)   = aux (i : difs)
    where difs = map (uncurry (-)) $ zip is (i:is)
          aux _ [] = []
          aux [] _ = []
          aux (d:ds) xs = case drop d xs of
                                []      -> []
                                (y:ys)  -> y : aux ds (y:ys)

--list must be ordered by index and mustn't contain repeated indexes
maybeIndexes :: (Num a, Eq a) => [(a,b)] -> [Maybe b]
maybeIndexes xs = rec 0 xs
    where rec _ [] = repeat Nothing
          rec n ((i, x) : t) | n == i    = Just x  : rec (n + 1) t
                             | otherwise = Nothing : rec (n + 1) ((i, x) : t)

repeatLast :: [a] -> [a]
repeatLast [x] = repeat x
repeatLast (x:xs) = x : repeatLast xs
repeatLast [] = error "repeatLast: expected non-empty list"

replaceLast :: (a -> a) -> [a] -> [a]
replaceLast f [x]    = [f x]
replaceLast f (x:xs) = x : replaceLast f xs
replaceLast _ []     = error "replaceLast: expected non-empty list"

findOrLast :: (a -> Bool) -> [a] -> a
findOrLast _ [x] = x
findOrLast p (x:xs) | p x = x
                    | otherwise = findOrLast p xs
findOrLast _ []  = error "findOrLast: expected non-empty list"

dropOrLast :: Int -> [a] -> [a]
dropOrLast _ [x] = [x]
dropOrLast 0 (x:xs) = x : xs
dropOrLast p (_:xs) = dropOrLast (p-1) xs
dropOrLast _ []     = error "dropOrLast: expected non-empty list"

indexOrLast :: [a] -> Int -> a
indexOrLast xs p = last $ take (p+1) xs

--when using, watch out for polymorphism
indexOrLastMemo :: [a] -> Int -> a
indexOrLastMemo = indexOrLast --TODO
{-
data LeafTree a = Leaf a | LNode (LeafTree a) | Node (LeafTree a) (LeafTree a)
indexOrLastMemo xs = search
  where trees = map (uncurry buildTree) $ splitPow xs
        splitPow xs = unfoldr aux (1,xs)
            where aux (n,[]) = Nothing
                  aux (n,ys) = let (l,r) = splitAt n ys in Just ((n,l),(2*n,r))
        buildTree _ [] = Empty
        buildTree n xs = Node (head r) (buildTree m l) (buildTree m (tail r))
          where m = n `div` 2
                (l,r) = splitAt m xs
        search n = indexTree (n + 1 - 2^i) (2^i) $ indexOrLast trees i
            where i = lg2 $ toInteger $ n + 1
        lastTree (Leaf x) = x
        lastTree (LNode t) = lastTree t
        lastTree (Node _ t) = lastTree t
        indexTree 0 _ (Leaf x) = x
        indexTree n m (Node l r) | 2 * n < m = indexTree (2 * n) m l
                                 | otherwise = indexTree (2 * n - m) m r
-}


--Balanced fold, minimizing depth of call tree. Assumes associative operator.
--This is useful for CompReals because operations usually try to balance errors by
--splitting it equally among the two terms. Therefore, if an operation is folded across
--a list, the first element will take half of the error, the second will take a quarter, etc
--This function ensures the error is distributed equally across all elements of the list
foldTree :: (a -> a -> a) -> a -> [a] -> a
foldTree _ u []        = u
foldTree f u (x:xs)    = foldTree f (f u x) (g xs)
  where g (x:y:xs)  = f x y : g xs
        g xs        = xs

-- Balanced fold for associative operator over non-empty list.
foldTree1 :: (a -> a -> a) -> [a] -> a
foldTree1 f (x:xs) = foldTree f x xs
foldTree1 _ []     = error "foldTree1: expected non-empty list"

--same as foldTree1, but generates all partial results (from the left)
--evaluating the partial results may require additional applications of f
-- (e.g. scanlTree1 (+) [1,2,3,4] will return [1, 1+2, (1+2)+3, (1+2)+(3+4)]
-- notice how the value (1+2)+3 isn't used in the next term)
scanlTree1 :: (a -> a -> a) -> [a] -> [a]
scanlTree1 f = map (thdOf3 . head) . tail . scanl (aux (0 :: Integer)) []
    where aux i [] x = [(x,i,x)]
          aux i ((y,j,acc):ys) x | i < j = (x,i,f x acc) : (y,j,acc) : ys
                                 | otherwise = aux (i+1) ys (f x y)