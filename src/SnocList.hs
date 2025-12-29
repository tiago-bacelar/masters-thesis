{-# LANGUAGE DeriveFunctor #-}

{-
This module represents lists as a body and a last element

This is useful when the last element of a (finite) list should be treated in some
special way (e.g. functions findOrLast and repeatLast)

Because of Haskell's laziness, infinite lists are represented as SnocList (...) _|_
Therefore, always keep in mind that evaluating the last element of a SnocList
is equivalent to iterating the full list that created it, and if that list is
infinite that operation will never halt
-}

module SnocList (
    SnocList,
    singleton,
    fromList,
    toList,
    foldr,
    unfoldr,
    head,
    last,
    drop,
    dropOrLast,
    zip,
    zip3,
    zipWith,
    zipWith3,
    iterate,
    indexOrLast,
    indexOrLastMemo,
    findOrLast,
    repeatLast,
    getIndexesOrLast) where

import Prelude hiding (foldr, head, last, take, drop, zip, zipWith, zip3, zipWith3, iterate)
import qualified Prelude as P
import Data.List (find)
import Data.Maybe (fromMaybe)


data SnocList a = SnocList [a] a deriving (P.Functor)

lFunc :: ([a] -> [a]) -> SnocList a -> SnocList a
lFunc f ~(SnocList xs y) = SnocList (f xs) y

singleton :: a -> SnocList a
singleton y = SnocList [] y

fromList :: [a] -> SnocList a
fromList [] = error "fromList: expected non-empty list"
fromList [x] = singleton x
fromList (x:xs) = lFunc (x:) (fromList xs)

toList :: SnocList a -> [a]
toList (SnocList xs y) = xs ++ [y]
--toList = foldr (:) singleton

foldr :: (a -> b -> b) -> (a -> b) -> SnocList a -> b
foldr f g (SnocList xs y) = P.foldr f (g y) xs

unfoldr :: (b -> Either a (a, b)) -> b -> SnocList a
unfoldr f = aux
    where aux x = either singleton (\(a,b) -> lFunc (a:) (aux b)) (f x)

head :: SnocList a -> a
head (SnocList (x:_) _) = x
head (SnocList [] y)    = y

last :: SnocList a -> a
last (SnocList _ y) = y

drop :: Int -> SnocList a -> [a]
drop i = P.drop i . toList

dropOrLast :: Int -> SnocList a -> SnocList a
dropOrLast i = lFunc (P.drop i)

zip :: SnocList a -> SnocList b -> SnocList (a,b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> SnocList a -> SnocList b -> SnocList c
zipWith f = aux
    where aux (SnocList (w:ws) x) (SnocList (y:ys) z) = lFunc (f w y :) $ aux (SnocList ws x) (SnocList ys z)
          aux wl1 wl2 = singleton $ f (head wl1) (head wl2)

zip3 :: SnocList a -> SnocList b -> SnocList c -> SnocList (a,b,c)
zip3 = zipWith3 (,,)

zipWith3 :: (a -> b -> c -> d) -> SnocList a -> SnocList b -> SnocList c -> SnocList d
zipWith3 f = aux
    where aux (SnocList (u:us) v) (SnocList (w:ws) x) (SnocList (y:ys) z) = lFunc (f u w y :) $ aux (SnocList us v) (SnocList ws x) (SnocList ys z)
          aux wl1 wl2 wl3 = singleton $ f (head wl1) (head wl2) (head wl3)

iterate :: (a -> a) -> a -> SnocList a
iterate f x = SnocList (P.iterate f x) undefined

indexOrLast :: SnocList a -> Int -> a
indexOrLast wl i = head $ dropOrLast i wl

--when using, watch out for polymorphism
indexOrLastMemo :: SnocList a -> Int -> a
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


findOrLast :: (a -> Bool) -> SnocList a -> a
findOrLast f (SnocList xs y) = fromMaybe y (find f xs)

repeatLast :: SnocList a -> [a]
repeatLast (SnocList xs y)  = xs ++ P.repeat y

--indexes must be increasing. if there are multiple indexes greater
--than the size of the list, the last element is only appended once
getIndexesOrLast :: [Int] -> SnocList a -> SnocList a
getIndexesOrLast []       = singleton . last
getIndexesOrLast (i:is)   = aux (i : difs)
    where difs = map (uncurry (-)) $ P.zip is (i:is)
          aux [] (SnocList _ y) = singleton y
          aux _ (SnocList [] y) = singleton y
          aux (d:ds) wl = case dropOrLast d wl of
                                (SnocList [] y)     -> singleton y
                                (SnocList (x:_) _)  -> lFunc (x:) (aux ds wl)