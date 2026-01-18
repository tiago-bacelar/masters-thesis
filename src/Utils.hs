{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils (module Utils) where

import Numeric (readSigned, readFloat)
import GHC.Num
import Data.Bits
import Data.Ratio
import Data.List.NonEmpty as NE (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
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

split :: (a -> b) -> (a -> c) -> a -> (b, c)
split f g x = (f x, g x)

(-|-) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
(-|-) f _ (Left x) = Left $ f x
(-|-) _ g (Right y) = Right $ g y
infix 4 -|-

(.-.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-.) f g x y = f (g x y)
infixr 9 .-.


thdOf4 :: (a, b, c, d) -> c
thdOf4 (_,_,x,_) = x


count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

lengthGreaterThan :: Int -> [a] -> Bool
lengthGreaterThan n = not . null . drop n

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs [] = xs
interleave [] ys = ys

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

strSplit :: (Eq a) => a -> [a] -> [[a]]
strSplit _ [] = []
strSplit s (x:xs) | s == x    = [] : strSplit s xs
                  | otherwise = case strSplit s xs of
                                    []      -> [[x]]
                                    ys:yss  -> (x : ys) : yss

strSplit2 :: (Eq a) => a -> [a] -> ([a],[a])
strSplit2 _ [] = ([], [])
strSplit2 s (x:xs) | s == x    = ([], xs)
                   | otherwise = let (ys, zs) = strSplit2 s xs in (x:ys, zs)

--lists must be ordered. f is applied to the same values multiple times (could be optimized)
mergeSortOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeSortOn _ xs [] = xs
mergeSortOn _ [] ys = ys
mergeSortOn f (x:xs) (y:ys) | f x <= f y = x : mergeSortOn f xs (y:ys)
                            | otherwise  = y : mergeSortOn f (x:xs) ys

--inserts an element into an ordered list. if the element already exists in the list, nothing changes
setInsert :: Ord a => a -> [a] -> [a]
setInsert x [] = [x]
setInsert x (h : t) | x < h = x : h : t
                    | x > h = h : setInsert x t
                    | otherwise = h : t

indexOrLength :: [a] -> Int -> Either a Int
indexOrLength xs i = foldr (\x r j -> case j of
                                        0 -> Left x
                                        _ -> r (j-1)) (Right . (i-)) xs i

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


leftDiagonals :: [[a]] -> [[a]]
leftDiagonals []           = []
leftDiagonals ([]:xss)     = leftDiagonals xss
leftDiagonals ((x:xs):xss) = [x] : zipWith (:) xs (leftDiagonals xss)



--Balanced fold, minimizing depth of call tree. Assumes associative operator.
--This is useful for CompReals because operations usually try to balance errors by
--splitting it equally among the two terms. Therefore, if an operation is folded across
--a list, the first element will take half of the error, the second will take a quarter, etc
--This function ensures the error is distributed equally across all elements of the list
foldTree :: (a -> a -> a) -> a -> [a] -> a
foldTree _ u []     = u
foldTree f u (x:xs) = foldTree f (f u x) (g xs)
    where g (y:z:zs)    = f y z : g zs
          g zs          = zs

-- Balanced fold for associative operator over non-empty list.
foldTree1 :: (a -> a -> a) -> [a] -> a
foldTree1 f (x:xs) = foldTree f x xs
foldTree1 _ []     = error "foldTree1: expected non-empty list"

--same as foldTree, but generates all partial results (from the left)
--evaluating the partial results may require additional applications of f
-- (e.g. scanlTree (+) 1 [2,3,4] will return [1, 1+2, (1+2)+3, (1+2)+(3+4)]
-- notice how the value (1+2)+3 isn't used in the next term)
scanlTree :: (a -> a -> a) -> a -> [a] -> [a]
scanlTree f x0 = map (thdOf3 . NE.head) . scanl (aux 0) ((x0, 0 :: Integer, x0) :| [])
    where aux i ((y,j,acc) :| ys) x | i < j = (x,i,f acc x) :| (y,j,acc) : ys
                                    | otherwise = case ys of
                                                    []   -> let z = f y x in (z, i+1, z) :| []
                                                    z:zs -> aux (i+1) (z:|zs) (f y x)

scanlTree1 :: (a -> a -> a) -> [a] -> [a]
scanlTree1 f (x:xs) = scanlTree f x xs
scanlTree1 _ []     = error "scanlTree1: expected non-empty list"


--for reading/writing decimal numbers
newtype Decimal = Decimal { toRat :: Rational } deriving (Num, Fractional, Enum)
instance Read Decimal where
    readsPrec _ = map (Decimal >< id) . readSigned readFloat
instance Show Decimal where
    showsPrec p (Decimal a) = showsPrec p (fromRational a :: Double) 

readDecimal :: String -> Rational
readDecimal = toRat . read